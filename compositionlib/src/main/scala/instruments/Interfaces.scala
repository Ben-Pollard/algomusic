package instruments

import enums.DrumNames
import midi.DevicesNames.DeviceName
import midi.FLStudioMIDIMap
import models.Primitives.PitchName
import models.Primitives.MidiPitch

abstract class Instrument {
  val channel: Int
  val port: DeviceName
  val voices: Int
}

abstract class TonalInstrument extends Instrument {
  val midiMap: () => Map[PitchName, MidiPitch]
  val lowestPitch: MidiPitch
  val highestPitch: MidiPitch
}

abstract class DrumKit extends Instrument {
  val pitchNameMap: Map[DrumNames.Value, PitchName]
  val midiPitchMap: Map[DrumNames.Value, MidiPitch]
}


