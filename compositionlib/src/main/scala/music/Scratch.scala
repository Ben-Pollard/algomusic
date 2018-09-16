package music

import midi.PatternPlayer
import primitives.Primitives._
import primitives.Mappings._

object Scratch extends App {
  var scale = Scale(majorScalePattern, midiNoteNames.get("C").get)
  val pitches = Seq(1,3,5,8).map(p => Pitch(scale, p))
//  val velocities = Seq.fill(pitches.length/2)(Velocities(Seq(120,60))).flatten
  val rhythm = Rhythm(4,Seq(1,2,3,4),Seq(q,q,q,q))
  val bar = Bar(pitches, rhythm)
  PatternPlayer(bar.notes, 100)
}
