package music

import midi.PatternPlayer
import primitives.Primitives._
import primitives.Mappings._

object Scratch extends App {
  var scale = Scale(majorScalePattern, midiNoteNames.get("C").get)
  val pitches = Seq(1,2,3,4,5,6,7,5,3,1).map(p => Pitch(scale, p))
  val rhythm = Rhythm(11,Seq(1,2,3,4,5,6,7,8,9,10,11),Seq(q,q,q,q,q,q,q,q,q,q,q))
  val bar = Bar(pitches, rhythm)
  PatternPlayer(bar.notes)
}
