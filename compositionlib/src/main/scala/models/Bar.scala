package models
import midi.DrumMap
import models.NoteSequences.{PolyphonicScalePhrase, PolyphonicScalePhraseBarConstructor, ScalePhrase, ScalePhraseBarConstructor}
import models.Primitives.{MidiCC, MidiCCValue, MidiMessageWithDuration, MidiNote, MidiPitch, NoteRest}

//A bar returns a sequence of sequences of notes, to be played in parallel
//Construct with one rhythm for chords, or multiple rhythms for more complex harmony
//Each sequence is monophonic - no overlaps between notes
case class Bar(messages: Seq[Seq[MidiMessageWithDuration]]) {

  def +(bar: Bar): Bar = {
    assert((messages ++ bar.messages).map(_.map(_.duration).sum).toSet.size == 1)
    Bar(messages ++ bar.messages)
  }
}


case class BarSequence(bars: Seq[Bar], channel: Int)

object Bar {
  //assumes number of non-rests are equal to number of pitches

//  case class Phrase(notes: Seq[Seq[MidiNote]])
//  type PolyphonicPhrase = List[Phrase]

  //constructor for bar of notes
  def apply(pitches: List[MidiPitch], rhythm: Rhythm): Bar = {

    assert(pitches.length == rhythm.velocities.length)
    assert(rhythm.durations.filter(_.isLeft).length == pitches.length)

    val onNotes = pitches zip rhythm.durations.filter(_.isLeft) zip rhythm.velocities map { n => MidiNote(Some(n._1._1), n._1._2.left.get, n._2) }

    val rests = rhythm.durations.filter(_.isRight).map(r => NoteRest(r.right.get))

    val notes = rests.zipAll(onNotes, NoteRest(0), NoteRest(0)).flatMap(pair => List(pair._1, pair._2))

    //correct any rounding errors in duration
    val roundingError = notes.map(_.duration).sum.round - notes.map(_.duration).sum
    val roundedNotes = notes.zipWithIndex.map(n => if (n._2== notes.length/2) n._1.copy(duration = n._1.duration + roundingError) else n._1)


    Bar(roundedNotes :: Nil)
  }


  def apply(drum: DrumNames.Value, rhythm: Rhythm): Bar = {
    val midiPitch: MidiPitch = DrumMap.fPC.get(drum).get
    val pitches = List.fill(rhythm.hitDurations.length)(midiPitch)
    apply(pitches, rhythm)
  }

  def apply(scalePhrase: ScalePhrase, rhythm: Rhythm): Bar = {
    val pitches = scalePhrase.degreeSequence.map(d => MidiPitch(scalePhrase.scale, d))
    apply(pitches, rhythm)
  }

  def apply(constructor: ScalePhraseBarConstructor): Bar = {
    apply(constructor.scalePhrase, constructor.rhythm)
  }

  def apply(constructor: PolyphonicScalePhraseBarConstructor): Bar = {
    Bar(constructor.scalePhrases.phrases.map{ m =>
      val mono: ScalePhrase = m
      apply(mono, constructor.rhythm)
    }.flatMap(n => n.messages))
  }

  def apply(scalePhrase: PolyphonicScalePhrase, rhythm: Rhythm): Bar = {
    Bar(scalePhrase.phrases.map{ m =>
      val mono: ScalePhrase = m
      apply(mono, rhythm)
    }.flatMap(n => n.messages))
  }

  //    def apply(pitches: Seq[Seq[MidiNote]], rhythm: Seq[Rhythm], velocities: Seq[Velocity]): Bar
  //
  //    def apply(pitches: Seq[MidiNote], rhythm: Seq[Rhythm], velocities: Seq[Velocity]): Bar
}
