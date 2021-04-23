package Transformers

import models.Primitives.{Bar, Phrase, PolyphonicPhrase}

object MelodyTransformers {

  def transpose(polyphonicPhrase: PolyphonicPhrase, transposeDegrees: Int): PolyphonicPhrase = {
    polyphonicPhrase.map(mono => {
      Phrase(mono.degreeSequence.map(d => d + transposeDegrees), mono.scale)
    })
  }

  def transpose(phrase: Phrase, transposeDegrees: Int): Phrase = {
    transpose(phrase :: Nil, transposeDegrees).head
  }

  def invert(phrase: Phrase): Phrase = {
    val s = phrase.degreeSequence
    val diffs = s.indices.tail.map(i => s(i) - s(i-1))
    Phrase(diffs.scanLeft(s.head)((a,b) => a-b).toList, phrase.scale)
  }

}
