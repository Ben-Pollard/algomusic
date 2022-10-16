package midi

import javax.sound
import javax.sound.midi.MidiSystem

object Devices extends Enumeration  {

  def getOutputDevice(name: String) = {
    val midiDevices = MidiSystem.getMidiDeviceInfo().toVector
    val midiOutDevices = midiDevices.filter(_.getClass.getName == "com.sun.media.sound.MidiOutDeviceProvider$MidiOutDeviceInfo")
    println(s"Available MIDI Out Devices: ${midiOutDevices.mkString("; ")}")
    try {
      val deviceInfo = midiOutDevices.find(_.getName()==name).get
      MidiSystem.getMidiDevice(deviceInfo)
    } catch {
      case e: java.lang.UnsupportedOperationException => throw new IllegalArgumentException(s"Specified device $name not found")
    }
  }

  def getSequencerDevice(): sound.midi.Sequencer = {
    val sequencer = MidiSystem.getSequencer()
    if (sequencer == null) {
      throw new IllegalArgumentException(s"Sequencer not found")
    }  else {
      sequencer
    }
  }
}