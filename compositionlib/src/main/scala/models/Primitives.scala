package models

object Primitives {

  type Velocity = Int
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
  val s: Duration = 0.0625
  val e: Duration = 0.125
  val q: Duration = 0.25
  val h: Duration = 0.5
  val w: Duration = 1
  val durations = List(s,e,q,h,w)

  type RestDuration = Double
  val rs: RestDuration = 0.0625
  val re: RestDuration = 0.125
  val rq: RestDuration = 0.25
  val rh: RestDuration = 0.5
  val rtq: RestDuration = 0.75
  val rw: RestDuration = 1


  type RhythmDurations = Seq[Either[Duration, RestDuration]]


  //NOTE
  case class MidiNote(pitch: Option[MidiPitch], duration:Duration, velocity: Velocity)

  object MidiNote {
    val v: Byte = 64
    def apply(pitch: Option[MidiPitch], duration:Duration = q, velocity: Velocity = v) = {
      new MidiNote(pitch, duration, velocity)
    }
  }

  object Rest {
    val v: Byte = 64
    def apply(duration: Duration) = new MidiNote(None, duration, v)
  }


  //PITCH CONSTRUCTORS
  object MidiPitch {
    def apply(scale: Scale, degree: Int):MidiPitch = {
      scale.getDegreePitch(degree)
    }
  }

//  object Direction extends Enumeration {
//    type Direction
//    val NONE, UP, DOWN = Value
//
//
//  }

  sealed abstract class DirectionBase
  case class Direction(name: String) extends DirectionBase {
    def opposite(): Direction = {
      Direction.this match {
        case UP => DOWN
        case DOWN => UP
        case NO_DIRECTION => NO_DIRECTION
      }
    }
  }
  object UP extends Direction("up")
  object DOWN extends Direction("down")
  object NO_DIRECTION extends Direction("none")

  object Direction {
    def apply(a:ScaleDegree, b:ScaleDegree): Direction = {
      if (b != a) {
        if (b > a) UP else DOWN
      } else {
        NO_DIRECTION
      }
    }
  }


}


