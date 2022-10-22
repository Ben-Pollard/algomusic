package instruments

import midi.DevicesNames.LOOP_MIDI_PORT
import midi.FLStudioMIDIMap
import models.Primitives.{MidiPitch, PitchName}

object CC_BBCSO extends Enumeration {
  val dynamics = Value(1)
  val gain = Value(7)
  val pan = Value(10)
  val expression = Value(11)
  val reverb = Value(19)
  val mix1 = Value(22)
}

abstract class BBCSO extends TonalInstrument {
  val port = LOOP_MIDI_PORT
  val lowestNoteName: String
  val highestNoteName: String
  val midiMap: () => Map[PitchName, MidiPitch] = () => FLStudioMIDIMap.midiMap
  lazy val lowestPitch: MidiPitch = midiMap().get(lowestNoteName).get
  lazy val highestPitch: MidiPitch = midiMap().get(highestNoteName).get

}

// Strings
case class Violin(voices: Int) extends BBCSO {
  val channel = 2
  val lowestNoteName = "G4"
  val highestNoteName = "C#8"
}

case class Viola(voices: Int) extends BBCSO {
  val channel = 2
  val lowestNoteName = "C4"
  val highestNoteName = "F#7"
}

case class Cello(voices: Int) extends BBCSO {
  val channel = 2
  val lowestNoteName = "C3"
  val highestNoteName = "A#6"
}

case class Contrabass(voices: Int) extends BBCSO {
  val channel = 2
  val lowestNoteName = "C2"
  val highestNoteName = "F#3"
}



// Brass
case class FrenchHorn(voices: Int) extends BBCSO {
  val channel = 2
  val lowestNoteName = "E3"
  val highestNoteName = "F6"
}

case class Trumpet(voices: Int) extends BBCSO {
  val channel = 2
  val lowestNoteName = "E4"
  val highestNoteName = "C7"
}

case class TenorTrombone(voices: Int) extends BBCSO {
  val channel = 2
  val lowestNoteName = "G2"
  val highestNoteName = "D6"
}

case class BassTrombone(voices: Int) extends BBCSO {
  val channel = 2
  val lowestNoteName = "E2"
  val highestNoteName = "G5"
}

case class Tuba(voices: Int) extends BBCSO {
  val channel = 2
  val lowestNoteName = "D2"
  val highestNoteName = "E5"
}



// Woodwind
case class Piccolo(voices: Int) extends BBCSO {
  val channel = 5
  val lowestNoteName = "D6"
  val highestNoteName = "C9"
}

case class Flute(voices: Int) extends BBCSO {
  val channel = 5
  val lowestNoteName = "B4"
  val highestNoteName = "C8"
}

case class Oboe(voices: Int) extends BBCSO {
  val channel = 6
  val lowestNoteName = "B4"
  val highestNoteName = "F7"
}

case class Clarinet(voices: Int) extends BBCSO {
  val channel = 6
  val lowestNoteName = "D4"
  val highestNoteName = "E7"
}

case class Bassoon(voices: Int) extends BBCSO {
  val channel = 6
  val lowestNoteName = "C#2"
  val highestNoteName = "D6"
}


// Percussion
case class Harp(voices: Int, lowestNoteName: String, highestNoteName: String) extends BBCSO {
  val channel = 7
  assert(lowestPitch >= midiMap().get("C3").get)
  assert(highestPitch <= midiMap().get("G9").get)
}