package music.polyrhythms

import generators.RhythmGenerators.bjorklund
import midi.{OutDevices, Sequencer}
import models.DrumNames.{KICK, SNARE}
import models.Primitives.q
import models.{Arrangement, Bar, BarSequence}
import util.Util.{lowestCommonMultiple, possibleTimeSigs}

object Runner extends App {

  val beatsPerBar = List(3,4)
  val totalSubdivs = 2 * lowestCommonMultiple(beatsPerBar)
  possibleTimeSigs(24)

  val _4 = bjorklund(totalSubdivs,4, q, List(100, 75, 75, 75)).beatsPerBar(4)
  val _3 = bjorklund(totalSubdivs,3, q, List(90, 75, 75)).beatsPerBar(4)

  val kick1 = Bar(KICK, _4 + _3)
  val kick = List.fill(2)(kick1)

  val sn1 = Bar(SNARE, _3)
  val sn = List.fill(2)(sn1)

  val drumLine = List(BarSequence(kick,1), BarSequence(sn,1))
  val arrangement = Arrangement(drumLine).repeat(1)

  Sequencer(arrangement, bpm = 120, midiDevice = OutDevices.LOOP_MIDI_PORT)

}
