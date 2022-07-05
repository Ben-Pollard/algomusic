package models

import models.Primitives.{Duration, RhythmDurations}
import models.NoteSequences.{PolyphonicScalePhrase, PolyphonicScalePhraseBarConstructor, ScalePhrase}
import models.Scales.CMaj

object NullObjects {
  //Use these for seeding a fold/scan/reduce
  val nullBar = Bar(messages = Seq(Seq()))

  val nullScalePhrase = ScalePhrase(List(), CMaj)

  def nullRhythm(totalSubdivs: Int) = Rhythm(1, totalSubdivs, Seq(), Seq(), Seq())

  def nullPolyphonicScalePhraseBarConstructor(totalSubdivs: Int) = PolyphonicScalePhraseBarConstructor(PolyphonicScalePhrase(List(nullScalePhrase)), nullRhythm(totalSubdivs))

  val nullChord = Chord(List(), CMaj)
}
