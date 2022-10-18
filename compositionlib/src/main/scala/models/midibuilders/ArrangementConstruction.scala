package models.midibuilders

import models.Primitives.ScaleDegree
import models.barconstructors.PolyphonicScalePhraseBarConstructor

object ArrangementConstruction {

  // Bar constructors with previous state for passing through to transformers and with arrangement control data
  case class BarInfo[A, B](oldConstructor: A, newConstructor: A, sequenceInfo: List[B])

  //
  case class SequenceInfo(rootDegree: ScaleDegree, chordNum: Int, barNum: Int)

  type BarConstructionAndSequencingData = List[BarInfo[PolyphonicScalePhraseBarConstructor, SequenceInfo]]
}
