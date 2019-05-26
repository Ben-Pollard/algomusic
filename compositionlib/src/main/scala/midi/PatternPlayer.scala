package midi

import javax.sound.midi.{MidiEvent, MidiSystem, Sequence, ShortMessage}
import models.Primitives.Duration
import org.jfugue.devices.MusicReceiver
import org.jfugue.pattern.Pattern
import org.jfugue.player.Player
import java.time._
import scala.collection.JavaConversions._

import sys.process._

object PatternPlayer {

  def getReceiverAndPlayer(): (MusicReceiver, Player) = {
    val devices = MidiSystem.getMidiDeviceInfo().toVector
    //JACK settings: monitor, hw out, no midi driver, alsa bridge running
    //patchbay settings: virmidi->carla midi; carla audio -> system audio
    "sudo modprobe snd-virmidi".! // get virtual devices to show up
    println(s"Devices: ${devices.mkString("; ")}")
    val chosenDeviceString = devices.filter(_.getName()=="VirMIDI [hw:3,0,0]").last
    val device = MidiSystem.getMidiDevice(chosenDeviceString)
    val receiver = new MusicReceiver(device)
    val player = new Player()
    (receiver, player)
  }

  def apply(pattern: Pattern): Unit = {
    val (receiver, player) = getReceiverAndPlayer()
    receiver.sendSequence(player.getSequence(pattern))
  }

  def apply(notes: Seq[models.Primitives.Note], bpm:Int): Unit = {

    val jFugueNotes = notes map{n =>
      new org.jfugue.theory.Note(n.pitch.getOrElse(0), n.duration)
        .setOnVelocity(n.velocity)
        .setRest(!n.pitch.isDefined)}

    val (receiver, player) = getReceiverAndPlayer()

    val pattern = new Pattern().setTempo(bpm)

    jFugueNotes.foreach(n => pattern.add(n))
    receiver.sendSequence(player.getSequence(pattern))
  }

}


object mySequencer {

  type Command = java.lang.Integer
  val noteOn: Command = 144
  val noteOff: Command = 128

  def apply(notes: Seq[models.Primitives.Note], bpm:Int) = {

    //init device
    val devices = MidiSystem.getMidiDeviceInfo().toVector
    val chosenDeviceString = devices.filter(_.getName()=="VirMIDI [hw:3,0,0]").last
    val device = MidiSystem.getMidiDevice(chosenDeviceString)
    device.open()
    val receiver = device.getReceiver

    //init sequence
//    var sequencer = MidiSystem.getSequencer
//    sequencer.open()
//    var sequence = new Sequence(Sequence.PPQ, 1, 1) //divisionType, resolution in PPQ (ticks per quarter note), numTracks


    val mySequence = notes.map {n =>
      val ms: Long = (1000 * 60 * n.duration *  1 / bpm).toLong
      if (n.pitch.isDefined) {
        //translate note duration to ms
        val onMessage = new ShortMessage()
        val offMessage = new ShortMessage()
        onMessage.setMessage(noteOn, 1, n.pitch.get, n.velocity) //command, channel, note, velocity
        offMessage.setMessage(noteOff, 1, n.pitch.get, n.velocity) //command, channel, note, velocity
        (Some(onMessage ,offMessage), ms)
      } else (None, ms)
    }

    for (m <- mySequence) {
      if (m._1.isDefined) {
        receiver.send(m._1.get._1, -1)
      }

      Thread.sleep(m._2)

      if (m._1.isDefined) {
        receiver.send(m._1.get._2, -1)
      }
    }

    receiver.close()
    device.close()

  }
}