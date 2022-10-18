package models.barconstructors

import models.Primitives.MidiCCValue
import models.Rhythm

case class CCBarConstructor(midiCCNum: midi.MidiControlNumbers.Value, midiCCValues: Seq[MidiCCValue], rhythm: Rhythm) {

}