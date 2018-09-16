package primitives

object Primitives {

  //
  type Velocity = Byte
  object Velocities {
    def apply(velocities: Seq[Int]):Seq[Velocity] = {
      velocities.map(_.asInstanceOf[Velocity])
    }
  }

  //TONE AND PITCH
  type Pitch = Int
  val midiRange: Seq[Pitch] = 0 to 127

  type Tone = Int
  val semiTone: Tone = 1
  val tone: Tone = 2


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
  type ScaleDegree = Int

  case class Scale(pitches: Seq[Pitch], root: Pitch, degreeMap: Map[ScaleDegree, Pitch]) {

    def getDegreePitch(d: Int) = {
      degreeMap.get(d).get
    }

    def getDegreeIndex(d:Int) = {
      pitches.indexOf(getDegreePitch(d))
    }

  }

  object Scale {
    def apply(pattern: Seq[Tone], root: Pitch):Scale = {
      assert(pattern.sum==12)

      val degreeMap =
        Seq.fill(2)(pattern.scanLeft(0)(_+_).take(7) map(_+root))
          .zipWithIndex
          .flatMap(s => s._1 map(_+s._2*12))
          .zipWithIndex map(p => (p._2 + 1, p._1)) toMap

      val pitches =
        Seq.fill(10)(pattern.scanRight(0)(_+_).takeRight(7).reverse)
        .zipWithIndex
        .flatMap(s => s._1 map(i => root - (i + s._2*12) ))
        .reverse
        .filter(n => n>=0 & n<root) ++
        Seq.fill(10)(pattern.scanLeft(0)(_+_).take(7) map(_+root))
          .zipWithIndex
          .flatMap(s => s._1 map(_+s._2*12))
          .filter(_<=midiRange.max)

      new Scale(pitches, root, degreeMap)
    }
  }





  //NOTE
  case class Note(pitch: Option[Pitch], duration:Duration, velocity: Velocity)

  object Rest {
    def apply(duration: Duration) = new Note(None, duration, 0)
  }

  object Note {
    def apply(scale:Scale, degree:Int, duration:Duration = q, velocity:Velocity = 64) = {
      new Note(Some(scale.getDegreePitch(degree)), duration, velocity)
    }
  }


  //PITCH CONSTRUCTORS
  object Pitch {
    def apply(scale: Scale, degree: Int):Pitch = {
      scale.getDegreePitch(degree)
    }
  }


  //SEQUENCES
  case class Bar(notes: Seq[Note]){}

  object Bar {
    //assumes number of non-rests are equal to number of pitches
    def apply(pitches: Seq[Pitch], rhythm: Rhythm, velocity: Velocity = 64.asInstanceOf[Velocity]): Bar = {
      val velocities = Seq.fill(pitches.length)(velocity)
      apply(pitches, rhythm, velocities)
    }

    def apply(pitches: Seq[Pitch], rhythm: Rhythm, velocities: Seq[Velocity]): Bar = {
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


