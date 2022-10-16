package models

import models.Primitives.ScaleDegree

object ArrangementConstruction {

  // Bar constructors with previous state for passing through to transformers and with arrangement control data
  case class BarInfo[A, B](oldConstructor: A, newConstructor: A, sequenceInfo: List[B])

  //
  case class SequenceInfo(rootDegree: ScaleDegree, chordNum: Int, barNum: Int)

  type BarConstructionAndSequencingData = List[BarInfo[PolyphonicScalePhraseBarConstructor, SequenceInfo]]
}
