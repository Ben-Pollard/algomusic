package models

import models.Primitives.{Duration, RhythmDurations}
import models.NoteSequences.{PolyphonicScalePhrase, PolyphonicScalePhraseBarConstructor, ScalePhrase}
import models.Scales.CMaj

object NullObjects {
  //Use these for seeding a fold/scan/reduce
  val nullBar = Bar(notes = Seq(Seq()))

  val nullScalePhrase = ScalePhrase(List(), CMaj)

  val nullRhythm = Rhythm(0, 0, Seq(), Seq(), Seq())

  val nullPolyphonicScalePhraseBarConstructor = PolyphonicScalePhraseBarConstructor(PolyphonicScalePhrase(List(nullScalePhrase)), nullRhythm)

  val nullChord = Chord(List(), CMaj)
}
