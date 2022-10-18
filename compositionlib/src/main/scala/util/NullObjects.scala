package util

import instruments.Instrument
import models.Primitives.{MessagePair, MessagePairWithTimestamp, MidiNote}
import models.Scales.CMaj
import models._
import models.barconstructors.PolyphonicScalePhraseBarConstructor
import models.midibuilders.Bar

object NullObjects {
  //Use these for seeding a fold/scan/reduce
  val nullNote = MidiNote(None, 0, 0)

  def nullBar(instrument: Instrument) = {
    Bar(voices = Seq(Seq()), instrument)
  }


  val nullMessagePair = MessagePair(None, 0)
  val nullMessagePairWithTimeStamp = MessagePairWithTimestamp(nullMessagePair, 0)

  val nullScalePhrase = ScalePhrase(List(), CMaj)

  def nullRhythm() = Rhythm(0, 0, Seq(), Seq(), Seq())

  def emptyRhythm(totalSubdivs: Int) = Rhythm(1, totalSubdivs, Seq(), Seq(), Seq())

  def nullPolyphonicScalePhraseBarConstructor(totalSubdivs: Int) = PolyphonicScalePhraseBarConstructor(PolyphonicScalePhrase(List(nullScalePhrase)), nullRhythm())

  val nullChord = Chord(List(), CMaj)
}
