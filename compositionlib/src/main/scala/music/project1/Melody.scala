package music.project1

import models.midibuilders.ArrangementConstruction.{BarConstructionAndSequencingData, BarInfo}
import models.Primitives._
import models._
import models.barconstructors.PolyphonicScalePhraseBarConstructor
import music.project1.Runner.scanSeed
import util.NoteFinder

object Melody {

  def apply(harmony: BarConstructionAndSequencingData): BarConstructionAndSequencingData = {

    //melody ideas
    //establishing the root; leaps; create an expectation; judicious use of repetition; contrapunctal movement wrt harmony
    //concordancy wrt harmony
    //start with a rhythm
    //step 1: contrary motion relative to the bar starts or to the changes?


    //Goals for this melody:
    //construct the phrase from the current outline note and the next outline note
    //harmonic logic for outline notes

    val harmonisedMelody = harmony.map(c => {
      val transposed = c.constructor.copy(scalePhrases = c.constructor.scalePhrases.transpose(14))
      BarInfo(transposed, c.controlData)
    }).scanLeft(scanSeed)((a, b) => {
      //In this scan we outline the melody
      val AEmpty = a.constructor.rhythm.beats == 0
      val firstDegreeBOld = b.constructor.scalePhrases.phrases.head.degreeSequence.head
      val firstDegreeAOld = if (AEmpty) firstDegreeBOld else a.constructor.scalePhrases.phrases.head.degreeSequence.head
      val firstDegreeBNew = b.constructor.scalePhrases.phrases.head.degreeSequence.head
      val firstDegreeANew = if (AEmpty) firstDegreeBNew else a.constructor.scalePhrases.phrases.head.degreeSequence.head

      val direction = Direction(firstDegreeAOld, firstDegreeBOld)
      val motion = NoteFinder.stepDegrees(direction.opposite, firstDegreeANew, 1)

      val roots = b.constructor.scalePhrases.phrases.take(1)

      val newPhrases = roots.map(p => {
        val degreeSequence = List(motion)
        p.copy(degreeSequence = degreeSequence)
      })

      val newRhythm = b.constructor.rhythm.take(1)


      val newConstructor = PolyphonicScalePhraseBarConstructor(PolyphonicScalePhrase(newPhrases), newRhythm)
      BarInfo(newConstructor, b.controlData)
    }).tail
      .scanRight(scanSeed)((a, b) => {
        //in this scan we colour it in
        val BEmpty = b.constructor.rhythm.beats == 0
        //we need to get to a barconstructor so we can call the infiller
        val targetNote = if (BEmpty) 1 else b.constructor.roots.scalePhrase.degreeSequence.head //start of next bar - if last bar, 1? idk, we should get this from a reduce or something
        val newConstructor = a.constructor.roots.scalePhraseRunFiller(a.constructor.rhythm, targetNote).toPoly
        BarInfo(newConstructor, a.controlData)
      })
      .init

    harmonisedMelody
  }
}
