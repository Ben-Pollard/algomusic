package primitives

import primitives.Primitives.{MidiNote, Scale, majorScalePattern}

object Mappings {

  val midiNoteNames: Map[String, MidiNote] = Map(
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

  val CMaj = new Scale(majorScalePattern, Mappings.midiNoteNames.get("C").get)


}
