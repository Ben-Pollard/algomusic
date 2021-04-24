package music

import generators.RhythmGenerators.bjorklund
import midi.{OutDevices, Sequencer}
import models.DrumNames.KICK
import models.Primitives.{Bar, BarSequence, ParallelBarSequences, Phrase, Rhythm, Velocity, h, q, w}
import music.Scratch.{arrangement, bar, bassBar, polyphonicPhrase, rhythm}

object DrumPattern extends App {

  //A phrase is a degree sequence and scale.
  // 1. Create a mapper that generates degree seq / scale from drum name + map
  //2. Supply rhythm an optional drum name and map
  //3. Create bar directly from Rhythm + drum name/map
  //4. Velocity is a facet of phrasing - how do velocity phrases work?
  //5. A bar is just a sequence of Midi notes

  val rhythm = bjorklund(9,5, hitDurations=Seq(w,h,q,w,q).map(_*2))
  val velocity: Velocity = 100.asInstanceOf[Velocity]
  val bar = Bar(KICK, rhythm, velocity)
  val arrangement: ParallelBarSequences = List(BarSequence(List(bar)))
  Sequencer(arrangement, 200, OutDevices.LOOP_MIDI_PORT)

}
