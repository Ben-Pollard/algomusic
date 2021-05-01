package models

import models.Mode.{Aeolian, Dorian, Ionian, Locrian, Lydian, Mixolydian, Phrygian}
import models.Primitives.{Tone, semiTone, tone}
import transformers.SequenceTransformers

object Mode extends Enumeration {
 type Mode
  val Ionian = Value(0)
  val Dorian = Value(1)
  val Phrygian = Value(2)
  val Lydian = Value(3)
  val Mixolydian = Value(4)
  val Aeolian = Value(5)
  val Locrian = Value(6)
}

object Scales {

  val majorScalePattern = Seq(tone,tone,semiTone,tone,tone,tone,semiTone)

  val modes = List(Ionian, Dorian, Phrygian, Lydian, Mixolydian, Aeolian, Locrian)

  val modeNameMap: Map[Mode.Value, Seq[Tone]] = modes.map(m => (m, SequenceTransformers.reverseRotate(majorScalePattern, m.id))).toMap

  val modeNumberMap: Map[Int, Seq[Tone]] = modeNameMap.map(m => (m._1.id, m._2))


  val CMaj = Scale(majorScalePattern, ControlSignals.midiNoteNames.get("C").get)


}
