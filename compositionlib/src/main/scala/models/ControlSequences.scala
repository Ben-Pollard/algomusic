package models

import midi.MidiControlNumbers
import models.Primitives.{MidiCCNum, MidiCCValue}

object ControlSequences {

  case class CCBarConstructor(midiCCNum: midi.MidiControlNumbers.Value, midiCCValues: Seq[MidiCCValue], rhythm: Rhythm) {

  }


}
