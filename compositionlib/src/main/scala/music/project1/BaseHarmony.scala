//package music.project1
//
//import enums.VoicingQualities
//import instruments.CC_BBCSO
//import models.Primitives.{h, q, w}
//import models.{PolyphonicScalePhrase, Rhythm, Scale, ScalePhrase}
//import models.barconstructors.PolyphonicScalePhraseBarConstructor
//import models.midibuilders.ArrangementConstruction.{BarConstructionAndSequencingData, BarInfo, SequenceInfo}
//import util.SequenceTransformers.rotate
//
//import scala.collection.parallel.ParSeq
//
//object BaseHarmony {
//  def apply(clave: Rhythm, scale: Scale, chordRoots: List[Int], chordDegrees: List[Int], sequenceIndices: ParSeq[List[SequenceInfo]]) = {
//
//    //Build up a chord sequence from the seed
//    sequenceIndices.map(s => {
//      val barNum = s.head.barNum
//      val pp = chordDegrees.map(d => ScalePhrase(s.map(i => i.rootDegree + d), scale))
//      val rhythm = {
//        val rotatedDurations = clave.copy(hitDurations = rotate(List(w, h, q, w, h).map(_ * 2), barNum + 4))
//        val alternateBarSwing = rotatedDurations.rotate(0, barNum % 2)
//        alternateBarSwing
//        clave.copy(hitDurations = List(w, h, q, w, h).map(_ * 1))
//      }.rotateVelocities(barNum) //hit a punctuation every 6
//      val barConstructor = PolyphonicScalePhraseBarConstructor(PolyphonicScalePhrase(pp), rhythm)
//        .applySineToEachNote(CC_BBCSO.expression.id)
//      BarInfo(barConstructor, barConstructor, s)
//    })
//
//  }
//}
