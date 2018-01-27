import javax.sound.midi.{MidiSystem, Sequencer}

import org.jfugue
import org.jfugue.devices.{MusicReceiver, MusicTransmitter}
import org.jfugue.pattern.Pattern
import org.jfugue.player.Player
import org.jfugue.theory.ChordProgression

object PatternPlayer {

  def apply(pattern: Pattern): Unit = {
    val devices = MidiSystem.getMidiDeviceInfo().toVector
    println(s"Devices: ${devices.mkString("; ")}")
    val chosenDeviceString = devices.filter(_.getDescription=="External MIDI Port").head
    //val chosenDeviceString = devices.filter(_.getName()=="Gervill").head
    val device = MidiSystem.getMidiDevice(chosenDeviceString)
    val receiver = new MusicReceiver(device)
    val player = new Player()
    receiver.sendSequence(player.getSequence(pattern))
  }

}
