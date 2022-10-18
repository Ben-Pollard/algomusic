package models.midibuilders

import enums.DrumNames
import instruments.{DrumKit, Instrument}
import models.Primitives.{MidiNote, MidiPitch, NoteRest, Voice}
import models._
import models.barconstructors.{PolyphonicScalePhraseBarConstructor, ScalePhraseBarConstructor}
import transformers.BarTransformers

//A bar returns a sequence of sequences of notes, to be played in parallel
//Construct with one rhythm for chords, or multiple rhythms for more complex harmony
//Each sequence is monophonic

abstract class AbstractBar(val voices: Seq[Voice], val instrument: Instrument)

case class Bar(override val voices: Seq[Voice], override val instrument: Instrument)
  extends AbstractBar(voices: Seq[Voice], instrument: Instrument)
    with BarTransformers


object Bar {
  //assumes number of non-rests are equal to number of pitches

  //constructor for bar of notes
  def apply(pitches: List[MidiPitch], rhythm: Rhythm, instrument: Instrument): Bar = {

    assert(pitches.length == rhythm.velocities.length)
    assert(rhythm.durations.filter(_.isLeft).length == pitches.length)

    val onNotes = pitches zip rhythm.durations.filter(_.isLeft) zip rhythm.velocities map { n => MidiNote(Some(n._1._1), n._1._2.left.get, n._2) }

    val rests = rhythm.durations.filter(_.isRight).map(r => NoteRest(r.right.get))

    val notes = rests.zipAll(onNotes, NoteRest(0), NoteRest(0))
      .flatMap(pair => {
        pair match {
          case (MidiNote(None, 0, _), MidiNote(None, 0, _)) => List(pair._1)
          case _ => List(pair._1, pair._2)
        }
      })

    //correct any rounding errors in duration
    val roundingError = notes.map(_.duration).sum.round - notes.map(_.duration).sum
    val roundedNotes = notes.zipWithIndex.map(n => if (n._2== notes.length/2) n._1.copy(duration = n._1.duration + roundingError) else n._1)


    Bar(roundedNotes :: Nil, instrument)
  }


  def apply(drum: DrumNames.Value, rhythm: Rhythm, instrument: DrumKit): Bar = {
    val midiPitch: MidiPitch = instrument.midiPitchMap.get(drum).get
    val pitches = List.fill(rhythm.hitDurations.length)(midiPitch)
    apply(pitches, rhythm, instrument)
  }

  def apply(scalePhrase: ScalePhrase, rhythm: Rhythm, instrument: Instrument): Bar = {
    val pitches = scalePhrase.degreeSequence.map(d => MidiPitch(scalePhrase.scale, d))
    apply(pitches, rhythm, instrument)
  }

  def apply(constructor: ScalePhraseBarConstructor, instrument: Instrument): Bar = {
    apply(constructor.scalePhrase, constructor.rhythm, instrument)
  }

  def apply(constructor: PolyphonicScalePhraseBarConstructor, instrument: Instrument): Bar = {
    Bar(constructor.scalePhrases.phrases.map{ m =>
      val mono: ScalePhrase = m
      apply(mono, constructor.rhythm, instrument)
    }.flatMap(n => n.voices), instrument)
  }

  def apply(scalePhrase: PolyphonicScalePhrase, rhythm: Rhythm, instrument: Instrument): Bar = {
    Bar(scalePhrase.phrases.map{ m =>
      val mono: ScalePhrase = m
      apply(mono, rhythm, instrument)
    }.flatMap(n => n.voices), instrument)
  }

}
