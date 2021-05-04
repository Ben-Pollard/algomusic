package music.project1

import models.{Arrangement, Bar, BarSequence, Motion}
import models.NoteSequences.{PolyphonicScalePhrase, PolyphonicScalePhraseBarConstructor, ScalePhrase}
import models.Primitives.{Direction, h, q, w}
import music.project1.Runner.{BarInfo, Project1SharedData, scanSeed}
import transformers.RhythmTransformers.shift
import transformers.SequenceTransformers.rotate

object CounterPoint {

  def apply(sd: Project1SharedData) = {

    //Build up a chord sequence from the seed
    val harmonicPhrasesWithSequenceIndexing = sd.sequenceIndices.map(s => {
      val barNum = s.head.barNum
      val pp = sd.chordDegrees.map(d => ScalePhrase(s.map(i => i.degree + d), sd.scale))
      val rhythm = {
        val rotatedDurations = sd.clave.copy(hitDurations = rotate(List(w, h, q, w, h).map(_ * 2), barNum + 4))
        val alternateBarSwing = shift(rotatedDurations, 0, barNum % 2)
        alternateBarSwing
      }
      val velocities = rotate(sd.claveVelocities, barNum) //hit a punctuation every 6
      val barConstructor = PolyphonicScalePhraseBarConstructor(PolyphonicScalePhrase(pp), rhythm, velocities)
      BarInfo(barConstructor, barConstructor, s)
    })


    //cantus firmus
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

      //Direction between first notes in bars
      val direction = Direction(firstA, firstB)

      //Pick a note - just taking 1 step in the opposite direction here
      //to develop the idea, we can think of how consonant we want the note to be based on the sequence info
      b.sequenceInfo.head.barNum
      val motion = Motion(direction.opposite, firstANew, 1)

      //Build up a bar constructor and pass it through with the original bar
      val scale = b.newConstructor.scalePhrases.phrases.head.scale
      val newPhrases = List(ScalePhrase(List(motion.degreeB), scale))
      val newRhythm = b.oldConstructor.rhythm.take(1)
      val newVelocities = b.oldConstructor.velocities.take(1)
      val newConstructor = PolyphonicScalePhraseBarConstructor(PolyphonicScalePhrase(newPhrases), newRhythm, newVelocities)
      BarInfo(b.oldConstructor, newConstructor, b.sequenceInfo)
    }).tail
//      .scanRight(scanSeed)((a, b) => {
//        //in this scan we colour it in
//        val BEmpty = b.oldConstructor.rhythm.beats == 0
//        //we need to get to a barconstructor so we can call the infiller
//        val targetNote = if (BEmpty) 1 else b.newConstructor.roots.scalePhrase.degreeSequence.head //start of next bar - if last bar, 1? idk, we should get this from a reduce or something
//        val newConstructor = a.newConstructor.roots.scalePhraseRunFiller(a.oldConstructor.rhythm, a.oldConstructor.velocities, targetNote).toPoly
//        BarInfo(a.oldConstructor, newConstructor, a.sequenceInfo)
//      })
//      .init
      .map(bi => Bar(bi.newConstructor))


    val melodyLine = List(BarSequence(harmonisedMelody, 5))
    Arrangement(melodyLine).repeat(2)
  }
}
