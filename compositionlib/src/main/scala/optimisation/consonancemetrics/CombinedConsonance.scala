package optimisation.consonancemetrics

import models.Primitives.{MidiPitch, PitchName}
import optimisation.consonancemetrics.Harmonicity.{getHarmonicity, getHarmonicityOfPitchNames}
import optimisation.consonancemetrics.Roughness.{getRoughnessOfPitchNames, getRoughnessOfPitches}

object CombinedConsonance {

  def getConsonanceOfPitchNames(pitchNames: List[PitchName]) = {
    val roughness = getRoughnessOfPitchNames(pitchNames)
    val harmonicity = getHarmonicityOfPitchNames(pitchNames)
    val roughnessCoeff = -1.6200
    val harmonicityCoeff = 1.7799
    val intercept = 0.6284
    val consonance = intercept + roughness*roughnessCoeff + harmonicity*harmonicityCoeff
    consonance
  }

  def getConsonanceOfPitches(pitchNames: List[MidiPitch]) = {
    val roughness = getRoughnessOfPitches(pitchNames)
    val harmonicity = getHarmonicity(pitchNames)
    val roughnessCoeff = -1.6200
    val harmonicityCoeff = 1.7799
    val intercept = 0.6284
    val consonance = intercept + roughness*roughnessCoeff + harmonicity*harmonicityCoeff
    consonance
  }

}
