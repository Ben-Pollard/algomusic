package Transformers

import models.Primitives.{Bar, Phrase, PolyphonicPhrase}

object MelodyTransformers {

  def transpose(polyphonicPhrase: PolyphonicPhrase, transposeDegrees: Int): PolyphonicPhrase = {
    polyphonicPhrase.map(mono => {
      Phrase(mono.degreeSequence.map(d => d + transposeDegrees), mono.scale)
    })
  }

}
