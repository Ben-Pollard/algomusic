package models
import midi.DrumMap
import models.NoteSequences.{PolyphonicScalePhrase, PolyphonicScalePhraseBarConstructor, ScalePhrase, ScalePhraseBarConstructor}
import models.Primitives.{MidiNote, MidiPitch, Rest}

//A bar returns a sequence of sequences of notes, to be played in parallel
//Construct with one rhythm for chords, or multiple rhythms for more complex harmony
//Each sequence is monophonic - no overlaps between notes
case class Bar(notes: Seq[Seq[MidiNote]])
case class BarSequence(bars: List[Bar], channel: Int)

object Bar {
  //assumes number of non-rests are equal to number of pitches

  case class Phrase(notes: Seq[Seq[MidiNote]])
  type PolyphonicPhrase = List[Phrase]

  def apply(pitches: List[MidiPitch], rhythm: Rhythm): Bar = {

    assert(pitches.length == rhythm.velocities.length)
    assert(rhythm.durations.filter(_.isLeft).length == pitches.length)

    val onNotes = pitches zip rhythm.durations.filter(_.isLeft) map { n => MidiNote(Some(n._1), n._2.left.get) }

    val rests = rhythm.durations.filter(_.isRight).map(r => Rest(r.right.get))

    val notes = rests.zipAll(onNotes, Rest(0), Rest(0)).flatMap(pair => List(pair._1, pair._2))

    Bar(notes :: Nil)
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
    }.flatMap(n => n.notes))
  }

  def apply(scalePhrase: PolyphonicScalePhrase, rhythm: Rhythm): Bar = {
    Bar(scalePhrase.phrases.map{ m =>
      val mono: ScalePhrase = m
      apply(mono, rhythm)
    }.flatMap(n => n.notes))
  }

  //    def apply(pitches: Seq[Seq[MidiNote]], rhythm: Seq[Rhythm], velocities: Seq[Velocity]): Bar
  //
  //    def apply(pitches: Seq[MidiNote], rhythm: Seq[Rhythm], velocities: Seq[Velocity]): Bar
}
