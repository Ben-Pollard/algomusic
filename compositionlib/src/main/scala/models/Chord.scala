package models

import instruments.TonalInstrument
import models.Primitives.{MidiPitch, ScaleDegree}
import transformers.ChordTransformers

abstract class AbstractChord(val scaleDegrees: List[ScaleDegree], val scale: Scale)

case class Chord(override val scaleDegrees: List[ScaleDegree], override val scale: Scale)
  extends AbstractChord(scaleDegrees: List[ScaleDegree], scale: Scale)
    with ChordTransformers {

  def getPitches(): List[MidiPitch] = {
    scaleDegrees.map(d => scale.degreeMap(d))
  }


}

