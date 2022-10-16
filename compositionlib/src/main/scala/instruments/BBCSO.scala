package instruments

import midi.DevicesNames.LOOP_MIDI_PORT
import midi.FLStudioMIDIMap
import midi.FLStudioMIDIMap.PitchName
import models.Primitives.MidiPitch

abstract class BBCSO extends TonalInstrument {
  val port = LOOP_MIDI_PORT
  val lowestNoteName: String
  val highestNoteName: String
  val midiMap: () => Map[PitchName, MidiPitch] = () => FLStudioMIDIMap.midiMap
  lazy val lowestPitch: MidiPitch = midiMap().get(lowestNoteName).get
  lazy val highestPitch: MidiPitch = midiMap().get(highestNoteName).get
}

case class Violins1(voices: Int) extends BBCSO {
  val channel = 2
  val lowestNoteName = "G4" //todo we've adjusted the names for violins only
  val highestNoteName = "C#8"
}

case class Flutes(voices: Int) extends BBCSO {
  val channel = 5
  val lowestNoteName = "B4"
  val highestNoteName = "C8"
}

case class Oboes(voices: Int) extends BBCSO {
  val channel = 6
  val lowestNoteName = "B4"
  val highestNoteName = "F7"
}

case class Harp(voices: Int, lowestNoteName: String, highestNoteName: String) extends BBCSO {
  val channel = 7
//  val lowestNoteName = "C3"
//  val highestNoteName = "G9"
  assert(lowestPitch >= midiMap().get("C3").get)
  assert(highestPitch <= midiMap().get("G9").get)
}