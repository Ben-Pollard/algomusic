package models

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
  type MidiNote = Int
  val midiRange: Seq[MidiNote] = 0 to 127

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


  case class Scale(pitches: Seq[MidiNote], root: MidiNote, degreeMap: Map[ScaleDegree, MidiNote]) {

    def getDegreePitch(d: Int) = {
      degreeMap.get(d).get
    }

    def getDegreeIndex(d:Int) = {
      pitches.indexOf(getDegreePitch(d))
    }

  }

  object Scale {
    def apply(pattern: Seq[Tone], root: MidiNote):Scale = {
      assert(pattern.sum==12)

      val degreeMap: Map[ScaleDegree, MidiNote] = Seq.fill(10)(pattern.scanLeft(0)(_+_).dropRight(1).map(_+root))
        .map(_.zipWithIndex).zipWithIndex
        .flatMap(i => i._1.map(j => (j._1 + 12*(i._2-5), j._2 + 1 + 7*(i._2-5))))
        .map(_.swap)
        .filter(x => x._2>0 & x._2<=midiRange.max)
        .toMap

      val degreeOctaveMap: Map[(ScaleDegree, Octave), MidiNote] = Seq.fill(10)(majorScalePattern.scanLeft(0)(_+_).dropRight(1).map(_+root))
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
  case class Note(pitch: Option[MidiNote], duration:Duration, velocity: Velocity)

  object Rest {
    def apply(duration: Duration) = new Note(None, duration, 0)
  }

  object Note {
    def apply(scale:Scale, degree:Int, duration:Duration = q, velocity:Velocity = 64) = {
      new Note(Some(scale.getDegreePitch(degree)), duration, velocity)
    }
  }


  //PITCH CONSTRUCTORS
  object MidiNote {
    def apply(scale: Scale, degree: Int):MidiNote = {
      scale.getDegreePitch(degree)
    }
  }


  //SEQUENCES
  case class Bar(notes: Seq[Note]){}

  object Bar {
    //assumes number of non-rests are equal to number of pitches
    def apply(pitches: Seq[MidiNote], rhythm: Rhythm, velocity: Velocity = 64.asInstanceOf[Velocity]): Bar = {
      val velocities = Seq.fill(pitches.length)(velocity)
      apply(pitches, rhythm, velocities)
    }

    def apply(pitches: Seq[MidiNote], rhythm: Rhythm, velocities: Seq[Velocity]): Bar = {
      assert(pitches.length == velocities.length)
      assert(rhythm.filter(_.isLeft).length == pitches.length)
      val pitchIterator = pitches.toIterator
      val velocityIterator = velocities.toIterator
      new Bar(rhythm map(n => if(n.isLeft) {
        Note(Some(pitchIterator.next()), n.left.get, velocityIterator.next())
      }
        else Note(None, n.right.get, 0.asInstanceOf[Velocity])))
    }
  }

  //class Scale(toStringnic: Note, mode: Mode)
}


