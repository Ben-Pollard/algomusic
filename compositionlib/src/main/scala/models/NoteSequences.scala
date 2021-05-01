package models

import models.Primitives.{ScaleDegree, Velocity}

object NoteSequences {

  case class PolyphonicScalePhrase(phrases: List[ScalePhrase]) {

    def transpose(transposeDegrees: Int): PolyphonicScalePhrase = {
      PolyphonicScalePhrase(phrases = phrases.map(mono => {
        ScalePhrase(mono.degreeSequence.map(d => d + transposeDegrees), mono.scale)
      }))
    }

  }

  //A phrase is monophonic
  case class ScalePhrase(degreeSequence: List[ScaleDegree], scale: Scale) {

  }


  //BAR CONSTRUCTORS
  case class ScalePhraseBarConstructor(scalePhrase: ScalePhrase, rhythm: Rhythm, velocities: Seq[Velocity])
  case class PolyphonicScalePhraseBarConstructor(scalePhrases: PolyphonicScalePhrase, rhythm: Rhythm, velocities: Seq[Velocity])

}
