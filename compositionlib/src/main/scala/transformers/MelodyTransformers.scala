package transformers

import models.NoteSequences.{PolyphonicScalePhrase, ScalePhrase}

object MelodyTransformers {

  def transpose(polyphonicScalePhrase: PolyphonicScalePhrase, transposeDegrees: Int): PolyphonicScalePhrase = {
    polyphonicScalePhrase.copy(phrases = polyphonicScalePhrase.phrases.map(mono => {
      ScalePhrase(mono.degreeSequence.map(d => d + transposeDegrees), mono.scale)
    }))
  }

  def transpose(scalePhrase: ScalePhrase, transposeDegrees: Int): ScalePhrase = {
    transpose(PolyphonicScalePhrase(scalePhrase :: Nil).phrases.head, transposeDegrees)
  }

  def invert(scalePhrase: ScalePhrase): ScalePhrase = {
    val s = scalePhrase.degreeSequence
    val diffs = s.indices.tail.map(i => s(i) - s(i-1))
    ScalePhrase(diffs.scanLeft(s.head)((a, b) => a-b).toList, scalePhrase.scale)
  }

}
