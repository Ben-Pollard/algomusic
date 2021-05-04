package models
import midi.DrumMap
import models.Primitives.{MidiNote, MidiPitch, Rest, Velocity}
import models.NoteSequences.{PolyphonicScalePhrase, PolyphonicScalePhraseBarConstructor, ScalePhrase, ScalePhraseBarConstructor}

//A bar returns a sequence of sequences of notes, to be played in parallel
//Construct with one rhythm for chords, or multiple rhythms for more complex harmony
//Each sequence is monophonic - no overlaps between notes
case class Bar(notes: Seq[Seq[MidiNote]])
case class BarSequence(bars: List[Bar], channel: Int)

object Bar {
  //assumes number of non-rests are equal to number of pitches

  case class Phrase(notes: Seq[Seq[MidiNote]])
  type PolyphonicPhrase = List[Phrase]

  def apply(pitches: List[MidiPitch], rhythm: Rhythm, velocities: Option[Seq[Velocity]]): Bar = {

    if (velocities.isDefined) {
      assert(pitches.length == velocities.get.length)
    }
    assert(rhythm.durations.filter(_.isLeft).length == pitches.length)

    val onNotes = if (!velocities.isDefined) {
      pitches zip rhythm.durations.filter(_.isLeft) map { n =>
        MidiNote(Some(n._1), n._2.left.get)
      }
    } else {
      pitches zip rhythm.durations.filter(_.isLeft) zip velocities.get map { n =>
        MidiNote(Some(n._1._1), n._1._2.left.get, n._2)
      }
    }

    val rests = rhythm.durations.filter(_.isRight).map(r => Rest(r.right.get))

    val notes = rests.zipAll(onNotes, Rest(0), Rest(0)).flatMap(pair => List(pair._1, pair._2))

    Bar(notes :: Nil)
  }

  def apply(drum: DrumNames.Value, rhythm: Rhythm, velocity: Velocity): Bar = {
    val midiPitch: MidiPitch = DrumMap.fPC.get(drum).get
    val pitches = List.fill(rhythm.hitDurations.length)(midiPitch)
    val velocities = List.fill(rhythm.hitDurations.length)(velocity)
    apply(pitches, rhythm, Some(velocities))
  }

  def apply(drum: DrumNames.Value, rhythm: Rhythm, velocities: Seq[Velocity]): Bar = {
    assert(rhythm.hitIndices.length == velocities.length)
    val midiPitch: MidiPitch = DrumMap.fPC.get(drum).get
    val pitches = List.fill(rhythm.hitDurations.length)(midiPitch)
    apply(pitches, rhythm, Some(velocities))
  }

  def apply(scalePhrase: ScalePhrase, rhythm: Rhythm, velocities: Option[Seq[Velocity]]): Bar = {
    val pitches = scalePhrase.degreeSequence.map(d => MidiPitch(scalePhrase.scale, d))
    apply(pitches, rhythm, velocities)
  }

  def apply(constructor: ScalePhraseBarConstructor): Bar = {
    apply(constructor.scalePhrase, constructor.rhythm, Some(constructor.velocities))
  }

  def apply(scalePhrase: ScalePhrase, rhythm: Rhythm, velocity: Velocity): Bar = {
    val velocities = Seq.fill(scalePhrase.degreeSequence.length)(velocity)
    apply(scalePhrase, rhythm, Some(velocities))
  }

  def apply(constructor: PolyphonicScalePhraseBarConstructor): Bar = {
    Bar(constructor.scalePhrases.phrases.map{ m =>
      val mono: ScalePhrase = m
      apply(mono, constructor.rhythm, Some(constructor.velocities))
    }.flatMap(n => n.notes))
  }

  def apply(scalePhrase: PolyphonicScalePhrase, rhythm: Rhythm): Bar = {
    Bar(scalePhrase.phrases.map{ m =>
      val mono: ScalePhrase = m
      apply(mono, rhythm, None)
    }.flatMap(n => n.notes))
  }

  //    def apply(pitches: Seq[Seq[MidiNote]], rhythm: Seq[Rhythm], velocities: Seq[Velocity]): Bar
  //
  //    def apply(pitches: Seq[MidiNote], rhythm: Seq[Rhythm], velocities: Seq[Velocity]): Bar
}
