package enums

import models.Primitives.MidiPitch

object MidiNoteNames extends Enumeration {

  val C = Value(60: MidiPitch)
  val C_# = Value(61: MidiPitch)
  val D = Value(62: MidiPitch)
  val D_# = Value(63: MidiPitch)
  val E = Value(64: MidiPitch)
  val F = Value(65: MidiPitch)
  val F_# = Value(66: MidiPitch)
  val G = Value(67: MidiPitch)
  val G_# = Value(68: MidiPitch)
  val A = Value(69: MidiPitch)
  val A_# = Value(70: MidiPitch)
  val B = Value(71: MidiPitch)

}
