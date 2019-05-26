package models

import models.Primitives.{Scale, semiTone, tone}

object Scales {

  val majorScalePattern = Seq(tone,tone,semiTone,tone,tone,tone,semiTone)

  val CMaj = Scale(majorScalePattern, ControlSignals.midiNoteNames.get("C").get)


}
