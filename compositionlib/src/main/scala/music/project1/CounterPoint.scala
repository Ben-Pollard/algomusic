package music.project1

import models.NoteSequences.{PolyphonicScalePhrase, PolyphonicScalePhraseBarConstructor, ScalePhrase}
import models.Primitives.{Direction, h, q, w}
import models.{Arrangement, Bar, BarSequence, NoteFinder}
import music.project1.Runner.{BarInfo, Project1SharedData, scanSeed}
import transformers.SequenceTransformers.rotate

object CounterPoint {

  def apply(sd: Project1SharedData) = {

    //cantus firmus
    val harmonicPhrasesWithSequenceIndexing = (sd.sequenceIndices ++ sd.sequenceIndices).map(s => {
      val barNum = s.head.barNum
      val pp = sd.chordDegrees.map(d => ScalePhrase(s.map(i => i.degree + d), sd.scale))
      val rhythm = {
        val rotatedDurations = sd.clave.copy(hitDurations = rotate(List(w, h, q, w, h).map(_ * 2), barNum + 4))
        val alternateBarSwing = rotatedDurations.rotate(0, barNum % 2)
        alternateBarSwing
      }.rotateVelocities(barNum) //hit a punctuation every 6
      val barConstructor = PolyphonicScalePhraseBarConstructor(PolyphonicScalePhrase(pp), rhythm)
      BarInfo(barConstructor, barConstructor, s)
    })

    val piano = harmonicPhrasesWithSequenceIndexing.map { c => Bar(c.oldConstructor) }


    val harmonisedMelody = harmonicPhrasesWithSequenceIndexing.map(c => {
      val transposed = c.oldConstructor.copy(scalePhrases = c.oldConstructor.scalePhrases.transpose(7))
      BarInfo(transposed, transposed, c.sequenceInfo)
    }).scanLeft(scanSeed)((a, b) => {
      //In this scan we outline the melody
      //establishing the root; leaps; create an expectation; concordancy wrt harmony
      //move a few iterations of the function to the bar constructor

      val AEmpty = a.oldConstructor.rhythm.beats == 0

      //The first root notes in the chords - original sequence
      val firstB = b.oldConstructor.scalePhrases.phrases.head.degreeSequence.head
      val firstA = if (AEmpty) firstB else a.oldConstructor.scalePhrases.phrases.head.degreeSequence.head

      //The first root notes in the chords - modified sequence
      val firstBNew = b.newConstructor.scalePhrases.phrases.head.degreeSequence.head
      val firstANew = if (AEmpty) firstBNew else a.newConstructor.scalePhrases.phrases.head.degreeSequence.head

      //Direction between first notes in original bars
      val direction = Direction(firstA, firstB)

      val scale = b.newConstructor.scalePhrases.phrases.head.scale

      //Pick a note - just taking 1 step in the opposite direction here
      //to develop the idea, we can think of how consonant we want the note to be based on the sequence info
      b.sequenceInfo.head.barNum
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
      val newRhythm = b.oldConstructor.rhythm.take(1)
      val newConstructor = PolyphonicScalePhraseBarConstructor(PolyphonicScalePhrase(newPhrases), newRhythm)
      BarInfo(b.oldConstructor, newConstructor, b.sequenceInfo)
    }).tail
      .scanRight(scanSeed)((a, b) => {
        //in this scan we colour it in
        val BEmpty = b.oldConstructor.rhythm.beats == 0
        //we need to get to a barconstructor so we can call the infiller
        val targetNote = if (BEmpty) 1 else b.newConstructor.roots.scalePhrase.degreeSequence.head //start of next bar - if last bar, 1? idk, we should get this from a reduce or something
        val newConstructor = a.newConstructor.roots.scalePhraseRunFiller(a.oldConstructor.rhythm, targetNote).toPoly
        BarInfo(a.oldConstructor, newConstructor, a.sequenceInfo)
      })
      .init
      .map(bi => Bar(bi.newConstructor))

    val pianoLine = List(BarSequence(piano, 2))
    val melodyLine = List(BarSequence(harmonisedMelody, 5))
    Arrangement(pianoLine ++melodyLine).repeat(1)
  }
}
