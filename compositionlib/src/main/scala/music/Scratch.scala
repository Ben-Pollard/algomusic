package music

import midi.{PatternPlayer, mySequencer}
import models.Primitives._
import models.ControlSignals._
import models.Scales._
import Generators.RhythmGenerators._
import Transformers.MelodyTransformers._

object Scratch extends App {
  var scale = Scale(majorScalePattern, midiNoteNames.get("C").get)
  val melody = List(1,3,6,7,5)
  val chordDegrees = List(1,3,5)
  val polyphonicPhrase = chordDegrees.map(d => Phrase(melody.map(m => m+d), scale))
  val transposed = transpose(polyphonicPhrase, -16)
  val rhythm = bjorklund(14,5, hitDurations=Seq(w,q,h,w,q))

  val phrases: PolyphonicPhrase = polyphonicPhrase ++ transposed
  val bar = Bar(phrases, rhythm)
  mySequencer(List.fill(3)(bar), 100)
}
