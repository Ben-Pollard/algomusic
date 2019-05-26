package models

import javax.sound.midi.ShortMessage
import models.Scales.majorScalePattern

object Primitives {

  //
  type Velocity = Byte
  object Velocities {
    def apply(velocities: Seq[Int]):Seq[Velocity] = {
      velocities.map(_.asInstanceOf[Velocity])
    }
  }

  //TONE AND PITCH
  type MidiPitch = Int
  val midiRange: Seq[MidiPitch] = 0 to 127

  type Tone = Int
  val semiTone: Tone = 1
  val tone: Tone = 2

  type ScaleDegree = Int
  type Octave = Int


  //DURATION AND RHYTHM
  type Duration = Double
  val q: Duration = 0.25
  val h: Duration = 0.5
  val w: Duration = 1

  type RestDuration = Double
  val rq: RestDuration = 0.25
  val rh: RestDuration = 0.5
  val rtq: RestDuration = 0.75
  val rw: RestDuration = 1


  type Rhythm = Seq[Either[Duration, RestDuration]]

  object Rhythm {
    def apply(divisions:Int, beatsAt: Seq[Int], noteDurations: Seq[Duration], beats:Int=4, numBars:Int=1):Rhythm = {
      assert(beatsAt.max <= divisions)
      assert(beatsAt.length == noteDurations.length)

      val stepLen:Duration = beats / divisions

      val nonOverlapNoteDurations: Seq[Duration] = (0 until beatsAt.length -1 map(i => {
        Vector((beatsAt(i+1) - beatsAt(i)) * stepLen, noteDurations(i)).min
      })) :+ Vector((divisions + 1 - beatsAt.last) * stepLen, noteDurations.last).min

      val restDurations: Iterator[RestDuration] = ((beatsAt.head - stepLen) +: ((0 until beatsAt.length -1) map(i => {
        ((beatsAt(i+1) - beatsAt(i)) * stepLen) - nonOverlapNoteDurations(i)
      })) :+ ((divisions + 1 - beatsAt.last) * stepLen) - nonOverlapNoteDurations.last).map(_.asInstanceOf[RestDuration]).toIterator

      val nonOverlapNoteDurationsIt = nonOverlapNoteDurations.toIterator

      1 to (beatsAt.length*2 + 1) map(b => if(b % 2 == 0) Left(nonOverlapNoteDurationsIt.next()) else Right(restDurations.next()))
    }
  }


  //SCALE


  case class Scale(pitches: Seq[MidiPitch], root: MidiPitch, degreeMap: Map[ScaleDegree, MidiPitch]) {

    def getDegreePitch(d: Int) = {
      degreeMap.get(d).get
    }

    def getDegreeIndex(d:Int) = {
      pitches.indexOf(getDegreePitch(d))
    }

  }

  object Scale {
    def apply(pattern: Seq[Tone], root: MidiPitch):Scale = {
      assert(pattern.sum==12)

      val degreeMap: Map[ScaleDegree, MidiPitch] = Seq.fill(10)(pattern.scanLeft(0)(_+_).dropRight(1).map(_+root))
        .map(_.zipWithIndex).zipWithIndex
        .flatMap(i => i._1.map(j => (j._1 + 12*(i._2-5), j._2 + 1 + 7*(i._2-5))))
        .map(_.swap)
        .filter(x => x._2>0 & x._2<=midiRange.max)
        .toMap

      val degreeOctaveMap: Map[(ScaleDegree, Octave), MidiPitch] = Seq.fill(10)(majorScalePattern.scanLeft(0)(_+_).dropRight(1).map(_+root))
        .map(_.zipWithIndex).zipWithIndex
        .flatMap(i => i._1.map(j => (j._1 + 12*(i._2-5), (j._2 + 1, (i._2-5)))))
        .map(_.swap)
        .filter(x => x._2>0 & x._2<=midiRange.max)
        .toMap


      val pitches = degreeMap.values.toList.sorted

      new Scale(pitches, root, degreeMap)
    }
  }





  //NOTE
  case class Note(pitch: Option[MidiPitch], duration:Duration, velocity: Velocity)

  object Note {
    val v: Byte = 64
    def apply(pitch: Option[MidiPitch], duration:Duration = q, velocity: Velocity = v) = {
      new Note(pitch, duration, velocity)
    }
  }

  object Rest {
    val v: Byte = 0
    def apply(duration: Duration) = new Note(None, duration, v)
  }



  //PITCH CONSTRUCTORS
  object MidiPitch {
    def apply(scale: Scale, degree: Int):MidiPitch = {
      scale.getDegreePitch(degree)
    }
  }


  //SEQUENCES
  type MonophonicPitchSequence = Seq[MidiPitch]
  type PolyphonicPitchSequence = Seq[MonophonicPitchSequence]

  //A bar returns a sequence of sequences of notes, to be played in parallel
  //Construct with one rhythm for chords, or multiple rhythms for more complex harmony
  //Each sequence is monophonic - no overlaps between notes
  case class Bar(notes: Seq[Seq[Note]])

  object Bar {
    //assumes number of non-rests are equal to number of pitches


    def apply(pitches: MonophonicPitchSequence, rhythm: Rhythm, velocities: Option[Seq[Velocity]]): Bar = {

      if (velocities.isDefined) {
        assert(pitches.length == velocities.get.length)
      }
      assert(rhythm.filter(_.isLeft).length == pitches.length)

      val onNotes = if (!velocities.isDefined) {
        pitches zip rhythm.filter(_.isLeft) map {n =>
            Note(Some(n._1), n._2.left.get)
        }
      } else {
        pitches zip rhythm.filter(_.isLeft) zip velocities.get map {n =>
            Note(Some(n._1._1), n._1._2.left.get, n._2)
        }
      }

      val rests = rhythm.filter(_.isRight).map(r => Rest(r.right.get))

      val notes = rests.zipAll(onNotes, Rest(0), Rest(0)).flatMap(pair => List(pair._1, pair._2))

      Bar(notes :: Nil)
    }

    def apply(pitches: MonophonicPitchSequence, rhythm: Rhythm, velocity: Velocity): Bar = {
      val velocities = Seq.fill(pitches.length)(velocity)
      apply(pitches, rhythm, Some(velocities))
    }

    def apply(pitches: PolyphonicPitchSequence, rhythm: Rhythm, velocities: Seq[Velocity]): Bar = {
      Bar(pitches.map{m =>
        val mono: MonophonicPitchSequence = m
        apply(mono, rhythm, Some(velocities))
      }.flatMap(n => n.notes))
    }

    def apply(pitches: PolyphonicPitchSequence, rhythm: Rhythm): Bar = {
      Bar(pitches.map{m =>
        val mono: MonophonicPitchSequence = m
        apply(mono, rhythm, None)
      }.flatMap(n => n.notes))
    }

//    def apply(pitches: Seq[Seq[MidiNote]], rhythm: Seq[Rhythm], velocities: Seq[Velocity]): Bar
//
//    def apply(pitches: Seq[MidiNote], rhythm: Seq[Rhythm], velocities: Seq[Velocity]): Bar
  }

  //class Scale(toStringnic: Note, mode: Mode)
}


