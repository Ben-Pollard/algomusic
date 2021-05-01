package midi

import midi.FLStudioMIDIMap.PitchName
import models.DrumNames
import models.DrumNames._
import models.Primitives.MidiPitch

object DrumMap {

  //Name, MidiPitch

  private val fPCPitchNames: Map[DrumNames.Value, PitchName] = Map(
    KICK -> "C3",
    SNARE -> "D3",
    HHC -> "F#3",
    HHO -> "A#3",
    HHP -> "G#3"
  )

  val fPC: Map[DrumNames.Value, MidiPitch] = fPCPitchNames
    .map { case (name, pitchname) => (name, FLStudioMIDIMap.midiMap.get(pitchname).get) }

}
