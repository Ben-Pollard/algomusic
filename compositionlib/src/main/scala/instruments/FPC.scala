package instruments

import enums.DrumNames
import enums.DrumNames._
import midi.DevicesNames.LOOP_MIDI_PORT
import midi.FLStudioMIDIMap
import models.Primitives.{MidiPitch, PitchName}

//FL Studio FPC instrument
object FPC extends DrumKit {
  val channel = 1
  val port = LOOP_MIDI_PORT
  val voices = 100
  val midiPitchMap: Map[DrumNames.Value, MidiPitch] = pitchNameMap.map { case (name, pitchname) => (name, FLStudioMIDIMap.midiMap.get(pitchname).get)}
  val pitchNameMap: Map[DrumNames.Value, PitchName] = Map(
    KICK -> "C3",
    SNARE -> "D3",
    HHC -> "F#3",
    HHO -> "A#3",
    HHP -> "G#3"
  )
}