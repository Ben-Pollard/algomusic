package music

import midi.Sequencer
import models.Primitives._
import models.ControlSignals._
import models.Scales._
import Generators.RhythmGenerators._
import Transformers.MelodyTransformers._
import Transformers.RhythmTransformers._

object Scratch extends App {
  var scale = Scale(majorScalePattern, midiNoteNames.get("C").get)
  val melody = List(1,3,6,7,5)
  val chordDegrees = List(1,3,5)

  val polyphonicPhrase = chordDegrees.map(d => Phrase(melody.map(m => m+d), scale))
  val rhythm = bjorklund(9,5, hitDurations=Seq(w,h,q,w,q).map(_*2))
  //val rhythm = Rhythm(5, List(0,1,2,3,4), Seq(w,h,q,w,q))
  val bar = Bar(polyphonicPhrase, rhythm)

  val bassPhrase = invert(transpose(Phrase(melody, scale), -16))
  val bassRhythm = shift(modifySubdivisions(rhythm, 3),1,1)
  val bassBar = Bar(bassPhrase :: Nil, bassRhythm)

//  val arrangement: ParallelBarSequences = List(BarSequence(List(bar)))
  val arrangement: ParallelBarSequences = List(BarSequence(List(bar)), BarSequence(List(bassBar)))
  Sequencer(arrangement, 200, "VirMIDI [hw:3,0,0]")
  //Sequencer(arrangement, 100)

  //todo: melody rhythm modulations - rotate by whole steps, shift over the beat
}
