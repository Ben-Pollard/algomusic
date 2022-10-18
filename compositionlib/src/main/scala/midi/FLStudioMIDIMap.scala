package midi

import models.Primitives.{MidiPitch, PitchName, midiRange}

object FLStudioMIDIMap extends MIDIMap {
  private val pitchNames = List("C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B")
  private val lowestPitchName = "C"
  private val highestPitchName = "G"
  private val pitchNumbers = 0 to 10
  private val noteOrder = pitchNames.zipWithIndex.toMap
  private val minPitchNum = pitchNumbers.min
  private val maxPitchNum = pitchNumbers.max

  val midiMap: Map[PitchName, MidiPitch] = pitchNumbers
    .flatMap(num => pitchNames.map(name => (name, num)))
    .filter { case (name: PitchName, num: Int) => {
      num match {
        case `minPitchNum` => noteOrder.get(name).get >= noteOrder.get(lowestPitchName).get
        case `maxPitchNum` => noteOrder.get(name).get <= noteOrder.get(highestPitchName).get
        case _ => true
      }
    }
    }
    .map { case (name, num) => s"$name$num" }
    .zipWithIndex
    .filter(midiNum => midiNum._2 >= midiRange.min & midiNum._2 <= midiRange.max)
    .toMap
}
