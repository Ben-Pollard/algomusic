package instruments

import midi.DevicesNames.LOOP_MIDI_PORT
import midi.FLStudioMIDIMap
import models.Primitives.{MidiPitch, PitchName, midiRange}

case class DefaultInstrument(channel: Int, voices: Int) extends TonalInstrument {
  val port = LOOP_MIDI_PORT
  val midiMap: () => Map[PitchName, MidiPitch] = () => FLStudioMIDIMap.midiMap
  val lowestPitch = midiRange.min
  val highestPitch = midiRange.max
}
