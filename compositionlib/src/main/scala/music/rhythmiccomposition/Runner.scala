package music.rhythmiccomposition

import enums.DrumNames._
import generators.Meter.{allSubdivisions, theOne}
import generators.RhythmGenerators.generateBjorklundSequence
import instruments.{LabsDrums, SSD5}
import midi.Sequencer
import models.midibuilders.{Arrangement, Bar, ControlBar, Track}


object Runner extends App {

  val kit = SSD5

  val bars = 1 to 2

  val drums = bars.map(bar => {
    val _5Over16 = generateBjorklundSequence(16, 5).setBeatsPerBar(4)
    val _3Over16 = generateBjorklundSequence(16, 7).setBeatsPerBar(4).rotate(1, 0)
    val sixteenths = allSubdivisions(4, 4)//.swing(1)

    Bar(KICK, theOne(4,4).swing(2), kit) +
      Bar(SNARE, theOne(4,4).rotate(1), kit) +
      Bar(HH_PEDAL, _5Over16, kit) +
      Bar(RIDE_BELL, _3Over16, kit) +
      Bar(HH_SHANK_LOOSEN, _5Over16.reverse.rotate(0,2), kit) +
    Bar(HH_SHANK_CLOSED_TIGHT, sixteenths.subtract(_5Over16).subtract(_3Over16), kit)
//      ControlBar(bar.con1structor.controlBarConstructor.get, kit)
  })
  val drumArrangement = Arrangement(List((Track(drums, kit))))

  Sequencer(drumArrangement).play(100, 2)

}
