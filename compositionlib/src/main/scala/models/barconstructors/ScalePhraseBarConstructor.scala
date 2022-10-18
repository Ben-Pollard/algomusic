package models.barconstructors

import models.{Rhythm, ScalePhrase}
import transformers.BarConstructorTransformers

abstract class AbstractScalePhraseBarConstructor(val scalePhrase: ScalePhrase, val rhythm: Rhythm)

case class ScalePhraseBarConstructor(override val scalePhrase: ScalePhrase, override val rhythm: Rhythm)
  extends AbstractScalePhraseBarConstructor(scalePhrase: ScalePhrase, rhythm: Rhythm)
  with BarConstructorTransformers
  {



}
