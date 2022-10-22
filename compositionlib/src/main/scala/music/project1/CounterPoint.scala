package music.project1

import models.midibuilders.ArrangementConstruction.{BarConstructionAndSequencingData, BarInfo}
import models.{PolyphonicScalePhrase, ScalePhrase, barconstructors}
import models.Primitives.Direction
import models.barconstructors.PolyphonicScalePhraseBarConstructor
import music.project1.Runner.scanSeed
import util.NoteFinder

object CounterPoint {

  def apply(harmony: BarConstructionAndSequencingData): BarConstructionAndSequencingData = {


    val harmonisedMelody = harmony.map(c => {
      val transposed = c.constructor.copy(scalePhrases = c.constructor.scalePhrases.transpose(14))
      BarInfo(transposed, c.controlData)
    }).scanLeft(scanSeed)((a, b) => {
      //In this scan we outline the melody
      //establishing the root; leaps; create an expectation; concordancy wrt harmony
      //move a few iterations of the function to the bar constructor

      val AEmpty = a.constructor.rhythm.beats == 0

      //The first root notes in the chords - original sequence
      val firstB = b.constructor.scalePhrases.phrases.head.degreeSequence.head
      val firstA = if (AEmpty) firstB else a.constructor.scalePhrases.phrases.head.degreeSequence.head

      //The first root notes in the chords - modified sequence
      val firstBNew = b.constructor.scalePhrases.phrases.head.degreeSequence.head
      val firstANew = if (AEmpty) firstBNew else a.constructor.scalePhrases.phrases.head.degreeSequence.head

      //Direction between first notes in original bars
      val direction = Direction(firstA, firstB)

      val scale = b.constructor.scalePhrases.phrases.head.scale

      //Pick a note - just taking 1 step in the opposite direction here
      //to develop the idea, we can think of how consonant we want the note to be based on the sequence info
      val stepOneOpposite = NoteFinder.stepDegrees(direction.opposite, firstANew, 1)

      // step: opposite direction; and fixed interval from root or fixed or consonant with preceding first note; consonant with chord
      // somehow we need to utilise methods on chord/motion/scalephrase
      val fixedIntervalFromRoot = {
        val interval = 5
        val intervalFrom = firstB
        val directFrom = firstANew
        firstB
        //find degree that satisfies: up/down from; is interval from
        NoteFinder.stepToDegree(direction.opposite, directFrom, intervalFrom, interval, scale)
      }

      //Build up a bar constructor and pass it through with the original bar

      val newPhrases = List(ScalePhrase(List(fixedIntervalFromRoot), scale))
      val newRhythm = b.constructor.rhythm.take(1)
      val newConstructor = barconstructors.PolyphonicScalePhraseBarConstructor(PolyphonicScalePhrase(newPhrases), newRhythm)
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
