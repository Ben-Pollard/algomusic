package models

import models.Primitives.ScaleDegree

//A phrase is monophonic
case class ScalePhrase(degreeSequence: List[ScaleDegree], scale: Scale) {

  def invert(): ScalePhrase = {
    val diffs = degreeSequence.indices.tail.map(i => degreeSequence(i) - degreeSequence(i - 1))
    ScalePhrase(diffs.scanLeft(degreeSequence.head)((a, b) => a - b).toList, scale)
  }

  def transpose(transposeDegrees: Int): ScalePhrase = {
    ScalePhrase(degreeSequence.map(d => d + transposeDegrees), scale)
  }
}
