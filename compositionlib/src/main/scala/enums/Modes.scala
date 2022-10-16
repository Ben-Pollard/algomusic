package enums

object Modes extends Enumeration {
  type Mode
  val Ionian = Value(0)
  val Dorian = Value(1)
  val Phrygian = Value(2)
  val Lydian = Value(3)
  val Mixolydian = Value(4)
  val Aeolian = Value(5)
  val Locrian = Value(6)
}
