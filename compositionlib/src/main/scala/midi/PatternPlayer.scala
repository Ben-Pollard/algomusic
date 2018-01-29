package midi

import javax.sound.midi.MidiSystem

import org.jfugue.devices.MusicReceiver
import org.jfugue.pattern.Pattern
import org.jfugue.player.Player

object PatternPlayer {

  def getReceiverAndPlayer(): (MusicReceiver, Player) = {
    val devices = MidiSystem.getMidiDeviceInfo().toVector
    println(s"Devices: ${devices.mkString("; ")}")
    //val chosenDeviceString = devices.filter(_.getDescription=="External MIDI Port").head
    val chosenDeviceString = devices.filter(_.getName()=="Gervill").head
    val device = MidiSystem.getMidiDevice(chosenDeviceString)
    val receiver = new MusicReceiver(device)
    val player = new Player()
    (receiver, player)
  }

  def apply(pattern: Pattern): Unit = {
    val (receiver, player) = getReceiverAndPlayer()
    receiver.sendSequence(player.getSequence(pattern))
  }

  def apply(notes: Seq[primitives.Primitives.Note], bpm:Int): Unit = {
    val jFugueNotes = notes map(n =>
      new org.jfugue.theory.Note(n.pitch.getOrElse(0), n.duration)
        .setOnVelocity(n.velocity)
        .setRest(!n.pitch.isDefined))
    val (receiver, player) = getReceiverAndPlayer()
    val pattern = new Pattern().setTempo(bpm)
    jFugueNotes.foreach(n => pattern.add(n))
    receiver.sendSequence(player.getSequence(pattern))
  }

}
