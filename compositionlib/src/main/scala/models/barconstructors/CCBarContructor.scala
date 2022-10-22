package models.barconstructors

import models.Primitives.{MidiCCNum, MidiCCValue}
import models.Rhythm

case class CCBarConstructor(midiCCNum: MidiCCNum, midiCCValues: Seq[MidiCCValue], rhythm: Rhythm) {

}