package models

import models.Primitives.MidiPitch

object FLStudioMIDIMap {
  val noteNames = List("C","C#","D","D#","E","F","F#","G","G#","A","A#","B")
  private val noteOrder = noteNames.zipWithIndex.toMap
  private val lowestNoteName = "C"
  private val highestNoteName = "G"
  type PitchName = String
  private val numbers = 0 to 8
  private val minNum = numbers.min
  private val maxNum = numbers.max

  val midiMap: Map[PitchName, MidiPitch] = numbers
    .flatMap(num => noteNames.map(name => (name, num)))
    .filter{ case (name: PitchName, num: Int) => {
      num match {
        case `minNum` => noteOrder.get(name).get >= noteOrder.get(lowestNoteName).get
        case `maxNum` => noteOrder.get(name).get <= noteOrder.get(highestNoteName).get
        case _ => true
      }
    }}
    .map{ case (name, num) => s"$name$num"}
    .zipWithIndex
    .toMap
}
