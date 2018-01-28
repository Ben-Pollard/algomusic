package primitives

object Primitives {

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
    def apply(divideBarInto:Int, beatsAt: Seq[Int], durations: Seq[Duration]):Rhythm = {
      assert(beatsAt.max <= divideBarInto)
      0 until divideBarInto map(b => if(beatsAt.contains(b)) Left(durations(b)) else Right((1/divideBarInto).asInstanceOf[RestDuration]))
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
  case class Note(pitch: Option[Pitch], duration:Duration)

  object Rest {
    def apply(duration: Duration) = new Note(None, duration)
  }

  object Note {
    def apply(scale: Scale, degree: Int, duration: Duration = q) = {
      new Note(Some(scale.getDegreePitch(degree)), duration)
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
    def apply(notes: Seq[Pitch], rhythm: Rhythm): Bar = {
      val pitchIterator = notes.toIterator
      new Bar(rhythm map(n => if(n.isLeft) Note(Some(pitchIterator.next()), n.left.get) else Note(None, n.right.get)))
    }
  }

  //class Scale(toStringnic: Note, mode: Mode)
}


