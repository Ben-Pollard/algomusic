package music.project1

import enums.VoicingQualities
import instruments.{Instrument, TonalInstrument}
import models.midibuilders.ArrangementConstruction.{BarConstructionAndSequencingData, BarInfo, SequenceInfo}
import models.Primitives._
import models._
import models.barconstructors.PolyphonicScalePhraseBarConstructor
import util.SequenceTransformers.rotate

import scala.collection.parallel.ParSeq

object Harmony {

  def apply(clave: Rhythm, scale: Scale, chordRoots: List[Int], chordDegrees: List[Int], sequenceIndices: ParSeq[List[SequenceInfo]]): BarConstructionAndSequencingData = {

    //Build up a chord sequence from the seed
    val harmonicPhrasesWithSequenceIndexing = sequenceIndices.map(s => {
      val barNum = s.head.barNum
      val pp = chordDegrees.map(d => ScalePhrase(s.map(i => i.rootDegree + d), scale))
      val rhythm = {
        val rotatedDurations = clave.copy(hitDurations = rotate(List(w, h, q, w, h).map(_ * 2), barNum + 4))
        val alternateBarSwing = rotatedDurations.rotate(0, barNum % 2)
        alternateBarSwing
        clave.copy(hitDurations = List(w, h, q, w, h).map(_*1))
      }.rotateVelocities(barNum) //hit a punctuation every 6
      val barConstructor = PolyphonicScalePhraseBarConstructor(PolyphonicScalePhrase(pp), rhythm).applySineToEachNote()
      BarInfo(barConstructor, barConstructor, s)
    })

    //VOICE LEADING
    //Voice leading needs to know where the chord changes are and have some lookahead/lookbehind
    //Once we start changing things, chord change locations might effectively change
    // - so we preserve the original locations for passing through to transformers e.g. harmony -> melody

    val revoiced: BarConstructionAndSequencingData = harmonicPhrasesWithSequenceIndexing.map(p => {
      val newConstructor = p.oldConstructor.revoice(
        staticRoot=false,
        allowInversions=true,
        voicingQuality = VoicingQualities.INTERNALLY_SPICY,
        qualityRank = 0,
        numVoices = scale.instrument.voices
      )
        .leading()

      p.copy(newConstructor = newConstructor, oldConstructor = newConstructor)
    }).toList

    revoiced
  }

}
