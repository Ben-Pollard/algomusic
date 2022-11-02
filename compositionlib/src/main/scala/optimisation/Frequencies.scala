package optimisation

import midi.FLStudioMIDIMap
import models.Primitives.{MidiPitch, PitchName, midiRange}

object EqualTemperament {
  val A440_Name: PitchName = "A5"
  val A440_Pitch: MidiPitch = FLStudioMIDIMap.midiMap.get(A440_Name).get
  val A440_Frequency: Double = 440.0
  val ratio: Double = math.pow(2.0, 1.0/12.0)

  val pitchName2Freq = FLStudioMIDIMap.midiMap
    .map(x => {
      val (name, pitch) = x
      val frequency = A440_Frequency * Math.pow(ratio, pitch - A440_Pitch)
      (name, frequency)
    })

  val pitchNum2Freq = (midiRange.min to midiRange.max)
    .map(pitch => {
      val frequency = A440_Frequency * Math.pow(ratio, pitch - A440_Pitch)
      (pitch, frequency)
    }).toMap
}
