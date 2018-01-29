package music

import midi.PatternPlayer
import primitives.Primitives._
import primitives.Mappings._

object Scratch extends App {
  var scale = Scale(majorScalePattern, midiNoteNames.get("C").get)
  val pitches = Seq(1,2,3,4,5,6).map(p => Pitch(scale, p))
  val velocities = Seq.fill(pitches.length/2)(Velocities(Seq(120,60))).flatten
  val rhythm = Rhythm(16,Seq(1,4,5,9,11,14),Seq(w,q,q,q,q,w))
  val bar = Bar(pitches, rhythm, velocities)
  PatternPlayer(bar.notes, 100)
}
