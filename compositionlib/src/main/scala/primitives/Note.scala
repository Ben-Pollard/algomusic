package primitives

object Primitives {


  val midiRange = Range(0,127)
  type MidiNote = Int

  type Tone = Int
  val semiTone: Tone = 1
  val tone: Tone = 2

  type Duration = Double
  val q: Duration = 0.25
  val h: Duration = 0.5
  val w: Duration = 1


  //SCALE
  class Scale(pattern: Seq[Tone], tonic: MidiNote) {
    assert(pattern.sum==12)
    val degrees = pattern.scanLeft(0)(_+_)
  }

  val majorScalePattern = Seq(tone,tone,semiTone,tone,tone,tone,semiTone)



  //NOTE
  case class Note(midiVal: Option[Int], duration:Duration)

  object Rest {
    def apply(duration: Duration) = new Note(None, duration)
  }

  object Note {
    def apply(scale: Scale, degree: Int, duration: Duration = q) = {
      new Note(Some(scale.degrees(degree)), duration)
    }
  }



  //RHYTHM
  type RestDuration = Double
  val rq: RestDuration = 0.25
  val rh: RestDuration = 0.5
  val rw: RestDuration = 1

  type Rhythm = Seq[Either[Duration, RestDuration]]

  object Rhythm {
    def apply(divideBarInto:Int, beatsAt: Seq[Int], durations: Seq[Duration]):Rhythm = {
      assert(beatsAt.max <= divideBarInto)
      Range(1,divideBarInto).map(b => if(beatsAt.contains(b)) Left(durations(b)) else Right((1/divideBarInto).asInstanceOf[RestDuration]))
    }
  }



  class Bar(numBeats: Int, notes: Seq[Note], rhythm: Rhythm){
    assert(notes.map(n => n.duration).sum == numBeats)
  }

  //class Scale(toStringnic: Note, mode: Mode)
}


