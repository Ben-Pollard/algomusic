package instruments

import enums.DrumNames
import enums.DrumNames._
import midi.DevicesNames.{DeviceName, LOOP_MIDI_PORT}
import midi.FLStudioMIDIMap
import midi.FLStudioMIDIMap.PitchName
import models.Primitives.{MidiPitch, midiRange}

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

case class DefaultInstrument(channel: Int, voices: Int) extends TonalInstrument {
  val port = LOOP_MIDI_PORT
  val midiMap: () => Map[PitchName, MidiPitch] = () => FLStudioMIDIMap.midiMap
  val lowestPitch = midiRange.min
  val highestPitch = midiRange.max
}

abstract class Drum extends Instrument {
  val pitchNameMap: Map[DrumNames.Value, PitchName]
  val midiPitchMap: Map[DrumNames.Value, MidiPitch] = pitchNameMap.map { case (name, pitchname) => (name, FLStudioMIDIMap.midiMap.get(pitchname).get) }
}

//FL Studio FPC instrument
object FPC extends Drum {
  val channel = 1
  val port = LOOP_MIDI_PORT
  val voices = 100
  val pitchNameMap: Map[DrumNames.Value, PitchName] = Map(
    KICK -> "C3",
    SNARE -> "D3",
    HHC -> "F#3",
    HHO -> "A#3",
    HHP -> "G#3"
  )
}

