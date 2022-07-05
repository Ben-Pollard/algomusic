package midi

import models.Primitives.{MidiCC, MidiNote}
import models.{Arrangement, BarSequence}

import javax.sound.midi.{MidiDevice, MidiSystem, ShortMessage}
import scala.collection.parallel.ForkJoinTaskSupport
import scala.concurrent.forkjoin.ForkJoinPool


object Sequencer {

  type Command = java.lang.Integer
  val noteOn: Command = 144
  val noteOff: Command = 128
  val controlChange: Command = 176


  type MessagePair = (Option[(ShortMessage, ShortMessage)], Long)
  type MonophonicSequence = Seq[MessagePair]
  type PolyphonicSequence = Seq[MonophonicSequence]
  type SequenceOfSequences = Seq[PolyphonicSequence]
  type MIDIArrangement = Seq[SequenceOfSequences]

  def getDevice(midiDevice: OutDevices.Value): MidiDevice = {
    val midiOutDevices = MidiSystem.getMidiDeviceInfo().toVector.filter(_.getClass.getName == "com.sun.media.sound.MidiOutDeviceProvider$MidiOutDeviceInfo")
    println(s"Available MIDI Out Devices: ${midiOutDevices.mkString("; ")}")

    try {
      val deviceInfo = midiOutDevices.find(_.getName()==midiDevice.toString).get
      MidiSystem.getMidiDevice(deviceInfo)
    } catch {
      case e: java.lang.UnsupportedOperationException => throw new IllegalArgumentException(s"Specified device $midiDevice not found")
    }

  }

  def apply(arrangement: Arrangement, bpm:Int, midiDevice: OutDevices.Value) = {

    //init receiver
    val device = getDevice(midiDevice)
    device.open()
    val receiver = device.getReceiver


    //init sequence
//    var sequencer = MidiSystem.getSequencer
//    sequencer.open()
//    var sequence = new Sequence(Sequence.PPQ, 1, 1) //divisionType, resolution in PPQ (ticks per quarter note), numTracks

    //We have 3 nested seqs. Outer=bars, they happen sequentially. Middle=seq of monophonic sequences, they happen concurrently
    val midiArrangement: MIDIArrangement = arrangement.barSequences.map { bars =>
      bars.bars.map { bar =>
        bar.messages.map { s =>
          val monophonicSequence = s.map { m =>
            val ms: Long = (m.duration * 60.0 * 1000.0 / (bpm.toDouble)).toLong
            m match {

              case MidiNote(pitch, duration, velocity) => {
                if (pitch.isDefined) {
                  //translate note duration to ms
                  val onMessage = new ShortMessage()
                  val offMessage = new ShortMessage()
                  onMessage.setMessage(noteOn, bars.channel-1, pitch.get, velocity) //command, channel, note, velocity
                  offMessage.setMessage(noteOff, bars.channel-1, pitch.get, velocity) //command, channel, note, velocity
                  (Some(onMessage, offMessage), ms)
                } else {
                  (None, ms)
                }
              }

              case MidiCC(duration, number, value) => {
                if (value.isDefined) {
                  val message = new ShortMessage()
                  message.setMessage(controlChange, bars.channel-1, number, value.get)
                  (Some(message, message), ms) //todo no need to send the message twice - change the monophonic sequencer
                } else {
                  (None, ms)
                }

              }
            }

          }
          monophonicSequence
        }
      }
    }


    def monophonicSequencer(monophonicSequence: MonophonicSequence): Unit = {
      for (m <- monophonicSequence) {
        if (m._1.isDefined) {
          receiver.send(m._1.get._1, -1)
        }

        Thread.sleep(m._2)

        if (m._1.isDefined) {
          receiver.send(m._1.get._2, -1)
        }
      }
    }

    //Plays the contents of a bar. Executes each phrase in parallel.
    def polyphonicSequencer(polyphonicSequence: PolyphonicSequence): Unit = {
      val pc = polyphonicSequence.par
      pc.tasksupport = new ForkJoinTaskSupport(new ForkJoinPool(polyphonicSequence.length))
      pc.map(s => monophonicSequencer(s))
    }

    //Plays sequences of bars
    def sequencePlayer(sequenceOfSequences: SequenceOfSequences): Unit = {
      for (sequence <- sequenceOfSequences) {
        polyphonicSequencer(sequence)
      }
    }

    //Plays the bar sequences in parallel
    def arrangementPlayer(midiArrangement: MIDIArrangement): Unit = {
      val parallelism = midiArrangement.map(s => s.map(p => p.length).max).max * midiArrangement.length
      val pc = midiArrangement.par
      pc.tasksupport = new ForkJoinTaskSupport(new ForkJoinPool(parallelism))
      pc.map(layer => sequencePlayer(layer))

    }


    arrangementPlayer(midiArrangement)

    receiver.close()
    device.close()

  }
}