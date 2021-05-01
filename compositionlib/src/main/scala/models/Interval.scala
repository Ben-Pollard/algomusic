package models

import models.Primitives.{Octave, Tone}

object Interval {

  //INTERVAL
//  type Interval = Int

  sealed abstract class IntervalBase
  case class Interval(tone: Tone, octave: Octave, name: String) extends IntervalBase

  object minor_second extends Interval(1, 0,"minor_second")
  object major_second extends Interval(2,0,"major_second")
  object minor_third extends Interval( 3,0,"minor_third")
  object major_third extends Interval(4,0,"major_third")
  object perfect_fourth extends Interval(5,0,"perfect_fourth")
  object augmented_fourth_or_diminished_fifth extends Interval(6,0,"augmented_fourth_or_diminished_fifth")
  object perfect_fifth extends Interval(7,0,"perfect_fifth")
  object minor_sixth extends Interval(8,0,"minor_sixth")
  object major_sixth extends Interval(9,0,"major_sixth")
  object minor_seventh extends Interval(10,0,"minor_seventh")
  object major_seventh extends Interval(11,0, "major_seventh")


}
