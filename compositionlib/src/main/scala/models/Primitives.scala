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
    def apply(steps:Int, hitIndices: Seq[Int], hitDurations: Seq[Duration]): Rhythm = {
      assert(hitIndices.max <= steps)
      assert(hitIndices.length == hitDurations.length)

//      val beatPattern = List.fill(steps)(0).zipWithIndex.map(x => if (hitIndices.contains(x._2)) 1 else 0)
//      val stepDuration: Duration = hitIndices.length.toDouble / steps
//      val restDuration: RestDuration = stepDuration
//      val patternAsDurations = beatPattern.map(x => if (x==1) Left(stepDuration) else Right(restDuration))
//      val accountForNoteLength = patternAsDurations.reduce((a,b) => {
//        (a.isLeft,b.isLeft) match {
//          case (true, true) => List(Left(List(a.left.get, stepDuration).max), b.left)
//          case (true, false) => List(a.left, b.right)
//          case (false, true) => List(Right(List(a.right.get, restDuration).max), b.left)
//          case (false, false) => List(a.right, b.right)
//
//        }
//      })

      val stepLen:Duration = hitIndices.length.toDouble / steps

      val nonOverlapHitDurations: Seq[Duration] = (0 until hitIndices.length -1 map(i => {
        Vector((hitIndices(i+1) - hitIndices(i)) * stepLen, hitDurations(i)).min
      })) :+ Vector((steps + 1 - hitIndices.last) * stepLen, hitDurations.last).min

      val restDurations: Seq[RestDuration] = ((hitIndices.head * -stepLen) +: ((0 until hitIndices.length -1) map(i => {
        ((hitIndices(i+1) - hitIndices(i)) * stepLen) - nonOverlapHitDurations(i)
      })) :+ ((steps - hitIndices.last) * stepLen) - nonOverlapHitDurations.last)

      assert(hitDurations.sum + "%.12f".format(restDurations.sum).toDouble == hitIndices.length)

      val nonOverlapNoteDurationsIt = nonOverlapHitDurations.toIterator
      val restDurationsIt = restDurations.toIterator

      1 to (hitIndices.length*2 + 1) map(b => if(b % 2 == 0) Left(nonOverlapNoteDurationsIt.next()) else Right(restDurationsIt.next()))
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
  case class MidiNote(pitch: Option[MidiPitch], duration:Duration, velocity: Velocity)

  object MidiNote {
    val v: Byte = 64
    def apply(pitch: Option[MidiPitch], duration:Duration = q, velocity: Velocity = v) = {
      new MidiNote(pitch, duration, velocity)
    }
  }

  object Rest {
    val v: Byte = 0
    def apply(duration: Duration) = new MidiNote(None, duration, v)
  }



  //PITCH CONSTRUCTORS
  object MidiPitch {
    def apply(scale: Scale, degree: Int):MidiPitch = {
      scale.getDegreePitch(degree)
    }
  }


  //SEQUENCES
  case class Phrase(degreeSequence: List[ScaleDegree], scale: Scale) // a phrase is monophonic
  type PolyphonicPhrase = List[Phrase]

  //A bar returns a sequence of sequences of notes, to be played in parallel
  //Construct with one rhythm for chords, or multiple rhythms for more complex harmony
  //Each sequence is monophonic - no overlaps between notes
  case class Bar(notes: Seq[Seq[MidiNote]])

  object Bar {
    //assumes number of non-rests are equal to number of pitches


    def apply(phrase: Phrase, rhythm: Rhythm, velocities: Option[Seq[Velocity]]): Bar = {

      val pitches = phrase.degreeSequence.map(d => MidiPitch(phrase.scale, d))

      if (velocities.isDefined) {
        assert(pitches.length == velocities.get.length)
      }
      assert(rhythm.filter(_.isLeft).length == pitches.length)

      val onNotes = if (!velocities.isDefined) {
        pitches zip rhythm.filter(_.isLeft) map { n =>
            MidiNote(Some(n._1), n._2.left.get)
        }
      } else {
        pitches zip rhythm.filter(_.isLeft) zip velocities.get map { n =>
            MidiNote(Some(n._1._1), n._1._2.left.get, n._2)
        }
      }

      val rests = rhythm.filter(_.isRight).map(r => Rest(r.right.get))

      val notes = rests.zipAll(onNotes, Rest(0), Rest(0)).flatMap(pair => List(pair._1, pair._2))

      Bar(notes :: Nil)
    }

    def apply(phrase: Phrase, rhythm: Rhythm, velocity: Velocity): Bar = {
      val velocities = Seq.fill(phrase.degreeSequence.length)(velocity)
      apply(phrase, rhythm, Some(velocities))
    }

    def apply(phrases: PolyphonicPhrase, rhythm: Rhythm, velocities: Seq[Velocity]): Bar = {
      Bar(phrases.map{ m =>
        val mono: Phrase = m
        apply(mono, rhythm, Some(velocities))
      }.flatMap(n => n.notes))
    }

    def apply(phrases: PolyphonicPhrase, rhythm: Rhythm): Bar = {
      Bar(phrases.map{ m =>
        val mono: Phrase = m
        apply(mono, rhythm, None)
      }.flatMap(n => n.notes))
    }

//    def apply(pitches: Seq[Seq[MidiNote]], rhythm: Seq[Rhythm], velocities: Seq[Velocity]): Bar
//
//    def apply(pitches: Seq[MidiNote], rhythm: Seq[Rhythm], velocities: Seq[Velocity]): Bar
  }

  //class Scale(toStringnic: Note, mode: Mode)
}


