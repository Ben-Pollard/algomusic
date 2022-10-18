package midi

import midi.Devices.{getOutputDevice, getSequencerDevice}
import midi.DevicesNames.DeviceName
import models.Primitives._
import models.midibuilders.Arrangement
import java.io.File
import javax.sound
import javax.sound.midi._
import scala.collection.JavaConverters._
import scala.collection.parallel.ForkJoinTaskSupport
import scala.concurrent.forkjoin.ForkJoinPool

case class ShortMessageWithDuration(shortMessage: Option[ShortMessage], duration: Duration)
case class ShortMessageWithTimestamp(shortMessage: Option[ShortMessage], timestamp: Duration)
case class ScalaMidiEvent(shortMessage: ShortMessage, tick: Long, timestamp: Duration)
case class ScalaMidiTrack(events: Seq[ScalaMidiEvent], joinNeighbours: Boolean, port: DeviceName)
case class ScalaSequence(sequence: Sequence, port: DeviceName)
case class SequencerInitResult(sequencerDevice: sound.midi.Sequencer, transmitter: Transmitter, outputDevice: MidiDevice, portName: String)

// Multiple sequences are required for using multiple output ports
case class Sequencer(sequences: Seq[ScalaSequence], ppq: Int) {

//  val outputDeviceName = DevicesNames.LOOP_MIDI_PORT

  //Plays sequences in parallel on separate output ports
  def play(bpm: Int, repeat: Int = 1) = {
    // Initiate the java sequencers in sequence
    val javaSequencerInitResults = sequences.map(s => initJavaSequencer(s, bpm, repeat))

    // Then execute in parallel
    val pc = javaSequencerInitResults.par
    pc.tasksupport = new ForkJoinTaskSupport(new ForkJoinPool(javaSequencerInitResults.length))
    pc.map(s => playOnPort(s))
  }

  def initJavaSequencer(sequence: ScalaSequence, bpm: Int, repeat: Int = 1): SequencerInitResult = {
    // set tempo midi meta message
    val tempo: BigInt = 1000000 * 60 / bpm
    val numerator: Byte = 4 //4 beats per bar
    val denominator: Byte = 4 //2**4=16th notes
    val clocks_per_click: Byte = 120
    val notated_32nd_notes_per_beat: Byte = 8
    val timeSig: Array[Byte] = Array(numerator, denominator, clocks_per_click, notated_32nd_notes_per_beat)

    for (track <- sequence.sequence.getTracks) {
      track.add(new MidiEvent(new MetaMessage(0x51, tempo.toByteArray, tempo.toByteArray.length), 0))
      track.add(new MidiEvent(new MetaMessage(0x58, timeSig, timeSig.length), 0))
    }

    val portName = sequence.port.toString

    //write file
    val fileType = MidiSystem.getMidiFileTypes(sequence.sequence)(0)
    val filePath = "C:\\Users\\benpo\\Documents\\Image-Line\\FL Studio\\Projects\\Midi\\"
    val fileName = s"hello_${portName}.midi"
    MidiSystem.write(sequence.sequence, fileType, new File(s"${filePath}${fileName}"))

    // wire up sequencer to midi port
    val sequencerDevice = getSequencerDevice()
    for (transmitter <- sequencerDevice.getTransmitters.asScala) {
      transmitter.close()
    }

    val transmitter = sequencerDevice.getTransmitter
    val outputDevice = getOutputDevice(portName)
    transmitter.setReceiver(outputDevice.getReceiver)

    // playback
    outputDevice.open()
    sequencerDevice.open()

    sequencerDevice.setSequence(sequence.sequence)
    sequencerDevice.setTempoInBPM(bpm)
    sequencerDevice.setLoopCount(repeat)

    SequencerInitResult(sequencerDevice, transmitter, outputDevice, portName)
  }

  def playOnPort(init: SequencerInitResult) = {
    init.sequencerDevice.start()
    println(s"Playing on port ${init.portName}")
    while (init.sequencerDevice.isRunning()) {
      Thread.sleep(1000)
    }
    println(s"Stopped playing on port ${init.portName}")
    init.transmitter.close()
    init.sequencerDevice.close()
    init.outputDevice.close()
  }

}

object Sequencer {

  val ppq = 960 //ticks per quarter note or beat //todo force note length to be int when multiplied by PPQ

  def apply(arrangement: Arrangement): Sequencer = {
    val startTimeMillis = System.currentTimeMillis()

    val tracks = arrangement.tracks.map { track =>
      val trackAsBarsWithEndTimes = track.bars.map(bar => {
        val voiceWithEndTime = bar.voices.map(voice => {
          //map voices to messages
          val messageEndTimes = voice.map(message => {
            val shortMessage = new ShortMessage()

            message match {
              //note on
              case MidiNote(pitch: Some[MidiPitch], duration, velocity) => {
                shortMessage.setMessage(noteOn, track.instrument.channel - 1, pitch.get, velocity) //command, channel, note, velocity
                ShortMessageWithDuration(Some(shortMessage), duration)
              }
              //note rest
              case MidiNote(None, duration, velocity) => {
                ShortMessageWithDuration(None, duration)
              }
              //midi control change info
              case MidiCC(duration, number, value: Some[MidiCCValue]) => {
                shortMessage.setMessage(controlChange, track.instrument.channel - 1, number, value.get)
                ShortMessageWithDuration(Some(shortMessage), duration)
              }
              //cc rest
              case MidiCC(duration, number, None) => {
                ShortMessageWithDuration(None, duration)
              }
            }
          })//now map durations to timestamps within each bar
            //first pass - calculate the end times
            .scanLeft(ShortMessageWithTimestamp(None, 0))((a,b) => {
              ShortMessageWithTimestamp(b.shortMessage, a.timestamp + b.duration)
            })
          //second pass - shift the end times back so they become start times
          val messageStartTimes = (messageEndTimes.map(_.shortMessage).tail zip messageEndTimes.map(_.timestamp).dropRight(1))
            .map(m => ShortMessageWithTimestamp(m._1, m._2))
          val filteredOutPrecedingRests = if (messageStartTimes.head == ShortMessageWithTimestamp(None, 0.0)) {
            messageStartTimes.tail
          } else {
            messageStartTimes
          }
            //infill the note rests to correspond to the preceding notes. control messages are always in a different voice to note messages
          val startTimesWithOffMessages = filteredOutPrecedingRests.scanLeft(ShortMessageWithTimestamp(None, 0))((a,b) => {
              b match {
                case ShortMessageWithTimestamp(shortMessage: Some[ShortMessage], timestamp) => b
                case ShortMessageWithTimestamp(None, timestamp) => {
                  val prevMessage = a.shortMessage.get
                  val offMessage = new ShortMessage()
                  offMessage.setMessage(noteOff, prevMessage.getChannel, prevMessage.getData1, prevMessage.getData2) //NB we set off velocity to equal on velocity
                  ShortMessageWithTimestamp(Some(offMessage), timestamp)
                }
              }
            }).tail
          //store the final end time
          (messageEndTimes.map(_.timestamp).max, startTimesWithOffMessages)
        })
        //flatten the bar into a bag of messages
        val barWithEndTime = (voiceWithEndTime.map(_._1).max, voiceWithEndTime.flatMap(_._2))
        barWithEndTime
        })//calculate the timestamps within the track
      val previousBarEndTimes = trackAsBarsWithEndTimes.map(_._1).scanLeft(0.0)((a,b) => a+b).dropRight(1)
      val events = (previousBarEndTimes zip trackAsBarsWithEndTimes.map(_._2))
        .map(x => {
          val (previousBarEnd, bar) = x
          bar.flatMap(message => {
            message match {
              case ShortMessageWithTimestamp(shortMessage: Some[ShortMessage], timestamp) => {
//                val tick = ((previousBarEnd + timestamp) * ppq).toLong
                val timeStampWithinTrack = previousBarEnd + timestamp
                val tick = (timeStampWithinTrack * ppq).toLong
                Some(ScalaMidiEvent(shortMessage.get, tick, timeStampWithinTrack))
              }
              case ShortMessageWithTimestamp(None, timestamp) => None
            }
          })
        }) //flatten bars into tracks
        .flatten

      ScalaMidiTrack(events, track.joinNeighbours, track.instrument.port)
    }

    // track-level postprocessing
    val postProcessedTracks = tracks.map(track => {

      if (track.joinNeighbours) {
        // note joins
        // arrange events into order based on time and note
        val sortedTrack = track.events.sortBy(event => (event.shortMessage.getChannel, event.shortMessage.getData1, event.timestamp))
        // identify if neighbouring events are an off/on pair and the on tick delta==0
        val deleteMarkers = sortedTrack.sliding(2)
          .map(pair => {
            pair(0).timestamp == pair(1).timestamp &&
              pair(0).shortMessage.getCommand == noteOff &&
              pair(1).shortMessage.getCommand == noteOn &&
              pair(0).shortMessage.getChannel == pair(1).shortMessage.getChannel &&
              pair(0).shortMessage.getData1 == pair(1).shortMessage.getData1
          }).toList

        val filter = ((false +: deleteMarkers) zip (deleteMarkers :+ false)).map(n => n._1 | n._2)

        //apply the filter
        val events = sortedTrack.zip(filter)
          .filterNot(_._2).map(_._1)
        track.copy(events = events)
      } else {
        track
      }

    })


    // Group the tracks by their output port and construct a java MIDI sequence for each group
    val sequences: Seq[ScalaSequence] = postProcessedTracks.groupBy(_.port).map(sequenceForPort => {
      val (device, trackList) = sequenceForPort
      val sequence = new Sequence(Sequence.PPQ, ppq)
      for (track <- trackList) {
        val midiTrack = sequence.createTrack()
        for (event <- track.events) {
          midiTrack.add(new MidiEvent(event.shortMessage, event.tick))
        }
      }
      ScalaSequence(sequence, device)
    }).toSeq

    val endTimeMillis = System.currentTimeMillis()
    val durationSeconds = (endTimeMillis - startTimeMillis)
    println(s"Built MIDI sequence in ${durationSeconds}ms")

    Sequencer(sequences, ppq)
  }

}
