package instruments

import enums.{DrumNames, MidiControlNumbers}
import midi.DevicesNames.DeviceName
import models.Primitives.{MidiPitch, PitchName}

abstract class Instrument extends MidiControlNumbers {
  val channel: Int
  val port: DeviceName
  val voices: Int
}

abstract class TonalInstrument extends Instrument {
  val midiMap: () => Map[PitchName, MidiPitch]
  val lowestPitch: MidiPitch
  val highestPitch: MidiPitch

  object CC
}

abstract class DrumKit extends Instrument {
  val pitchNameMap: Map[DrumNames.Value, PitchName]
  val midiPitchMap: Map[DrumNames.Value, MidiPitch]
}


