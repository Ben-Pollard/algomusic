package primitives

import primitives.Primitives._

object Mappings {

  val midiNoteNames: Map[String, Pitch] = Map(
    "C" -> 60,
    "C#" -> 61,
    "D" -> 62,
    "D#" -> 63,
    "E" -> 64,
    "F" -> 65,
    "F#" -> 66,
    "G" -> 67,
    "G#" -> 68,
    "A" -> 69,
    "A#" -> 70,
    "B" -> 71
  )

  val majorScalePattern = Seq(tone,tone,semiTone,tone,tone,tone,semiTone)

  val CMaj = Scale(majorScalePattern, Mappings.midiNoteNames.get("C").get)


}
