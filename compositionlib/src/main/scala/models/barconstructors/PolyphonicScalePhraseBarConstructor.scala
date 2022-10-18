package models.barconstructors

import models._
import transformers.{ControlBarConstructorTransformers, PolyphonicBarConstructorTransformers}

abstract class AbstractPolyphonicScalePhraseBarConstructor(val scalePhrases: PolyphonicScalePhrase,
                                                           val rhythm: Rhythm,
                                                           val controlBarConstructor: Option[CCBarConstructor])

case class PolyphonicScalePhraseBarConstructor(override val scalePhrases: PolyphonicScalePhrase,
                                               override val rhythm: Rhythm,
                                               override val controlBarConstructor: Option[CCBarConstructor] = None)
  extends AbstractPolyphonicScalePhraseBarConstructor(scalePhrases: PolyphonicScalePhrase,rhythm: Rhythm, controlBarConstructor: Option[CCBarConstructor])
    with PolyphonicBarConstructorTransformers
    with ControlBarConstructorTransformers
  {


}
