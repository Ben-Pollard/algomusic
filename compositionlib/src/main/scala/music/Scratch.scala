package music

import midi.{PatternPlayer, mySequencer}
import models.Primitives._
import models.ControlSignals._
import models.Scales._

object Scratch extends App {
  var scale = Scale(majorScalePattern, midiNoteNames.get("C").get)
  val degrees1 = Seq(1,3,5,8).map(d => MidiPitch(scale, d))
  val degrees2 = Seq(5,7,9,12).map(d => MidiPitch(scale, d))
//  val velocities = Seq.fill(pitches.length/2)(Velocities(Seq(120,60))).flatten
  val rhythm = Rhythm(divisions = 4, beatsAt=Seq(1,2,3,4), noteDurations=Seq(q,q,q,q), beats=4, numBars=1)
  val bar = Bar(degrees1 :: degrees2 :: Nil, rhythm)
  mySequencer(bar :: Nil, 60)
}
