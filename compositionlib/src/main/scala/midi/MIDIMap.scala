package midi

import models.Primitives.{MidiPitch, PitchName}

abstract class MIDIMap {
  val midiMap: Map[PitchName, MidiPitch]
}
