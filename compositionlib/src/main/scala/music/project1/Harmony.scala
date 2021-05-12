package music.project1

import models.NoteSequences.{PolyphonicScalePhrase, PolyphonicScalePhraseBarConstructor, ScalePhrase}
import models.Primitives._
import models._
import music.project1.Runner.{BarInfo, Project1SharedData, scanSeed}
import transformers.SequenceTransformers.rotate

object Harmony {

  def apply(sd: Project1SharedData) = {

    //Build up a chord sequence from the seed
    val harmonicPhrasesWithSequenceIndexing = sd.sequenceIndices.map(s => {
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

    //VOICE LEADING
    //Voice leading needs to know where the chord changes are and have some lookahead/lookbehind
    //Once we start changing things, chord change locations might effectively change - let's preserve the original locations



    val revoiced = harmonicPhrasesWithSequenceIndexing.map(p => {
      val newConstructor = p.oldConstructor.revoice()
      p.copy(oldConstructor = newConstructor, newConstructor = newConstructor)
    })
    //    .scanLeft(scanSeed)((a,b) => { //between-bar voicings
    //      b.
    //    })

    val piano = revoiced.map { c => Bar(c.oldConstructor) }



    //melody ideas
    //establishing the root; leaps; create an expectation; judicious use of repetition; contrapunctal movement wrt harmony
    //concordancy wrt harmony
    //start with a rhythm
    //step 1: contrary motion relative to the bar starts or to the changes?


    //Goals for this melody:
    //construct the phrase from the current outline note and the next outline note
    //harmonic logic for outline notes

    val harmonisedMelody = revoiced.map(c => {
      val transposed = c.oldConstructor.copy(scalePhrases = c.oldConstructor.scalePhrases.transpose(7))
      BarInfo(transposed, transposed, c.sequenceInfo)
    }).scanLeft(scanSeed)((a, b) => {
      //In this scan we outline the melody
      val AEmpty = a.oldConstructor.rhythm.beats == 0
      val firstDegreeBOld = b.oldConstructor.scalePhrases.phrases.head.degreeSequence.head
      val firstDegreeAOld = if (AEmpty) firstDegreeBOld else a.oldConstructor.scalePhrases.phrases.head.degreeSequence.head
      val firstDegreeBNew = b.newConstructor.scalePhrases.phrases.head.degreeSequence.head
      val firstDegreeANew = if (AEmpty) firstDegreeBNew else a.newConstructor.scalePhrases.phrases.head.degreeSequence.head

      val direction = Direction(firstDegreeAOld, firstDegreeBOld)
      val motion = NoteFinder.stepDegrees(direction.opposite, firstDegreeANew, 1)

      val roots = b.oldConstructor.scalePhrases.phrases.take(1)

      val newPhrases = roots.map(p => {
        val degreeSequence = List(motion)
        p.copy(degreeSequence = degreeSequence)
      })

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
    //  val bassLine = List(BarSequence(bass, 3))
    val melodyLine = List(BarSequence(harmonisedMelody, 5))
    Arrangement(pianoLine ++ melodyLine).repeat(2)
  }
}
