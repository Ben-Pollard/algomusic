package models

import models.DrumNames.{HHC, HHO, HHP, KICK, SNARE}
import models.Primitives.{MidiNote, MidiPitch}
import models.FLStudioMIDIMap.PitchName

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
    .map{ case (name, pitchname) => (name, FLStudioMIDIMap.midiMap.get(pitchname).get)}

}
