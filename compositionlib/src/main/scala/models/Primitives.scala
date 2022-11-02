package models

import javax.sound.midi.ShortMessage

object Primitives {

  // PHYSICS
  type Amplitude = Double
  type Frequency = Double

  type MidiCCValue = Int
  type MidiCCNum = Int

  type Velocity = Int
  object Velocities {
    def apply(velocities: Seq[Int]):Seq[Velocity] = {
      velocities.map(_.asInstanceOf[Velocity])
    }
  }

  //TONE AND PITCH
  type MidiPitch = Int
  val midiRange: Seq[MidiPitch] = 0 to 127

  // C0, C#0, .. C1 etc
  type PitchName = String

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

  trait MidiMessageWithDuration {
    val duration: Duration
  }

  type Voice = Seq[MidiMessageWithDuration]

  case class MidiCC(duration: Duration, number: MidiCCNum, value: Option[MidiCCValue]) extends MidiMessageWithDuration
  //NOTE
  case class MidiNote(pitch: Option[MidiPitch], duration: Duration, velocity: Velocity) extends MidiMessageWithDuration

  object NoteRest {
    val v: Byte = 64
    def apply(duration: Duration) = MidiNote(None, duration, v)
  }

  object CCRest {
    val n: Byte = 0
    def apply(duration: Duration) = MidiCC(duration, n, None)
  }

//MIDI SEQUENCING
  type Command = java.lang.Integer
  val noteOn: Command = 144
  val noteOff: Command = 128
  val controlChange: Command = 176
  case class MessageWithTimeStamp(message: ShortMessage, timeStamp: Long)
  case class MessagePair(messagePair: Option[(ShortMessage, ShortMessage)], waitDuration: Long)
  case class MessagePairWithTimestamp(messagePair: MessagePair, timestamp: Long)
  type MidiMonophonicBar = Seq[MessagePair]
  type MidiPolyphonicBar = Seq[MidiMonophonicBar]
  type MidiTrack = Seq[MidiPolyphonicBar]


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


