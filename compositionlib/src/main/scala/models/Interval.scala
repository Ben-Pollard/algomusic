package models

import models.Primitives.{Octave, ScaleDegree, Tone}

object Interval {

  //INTERVAL
//  type Interval = Int

  sealed abstract class IntervalBase
  case class Interval(tone: Tone, octave: Octave, name: String, dissonanceRank: Int) extends IntervalBase

  object unison extends Interval(0, 0,"unison", 0)
  object minor_second extends Interval(1, 0,"minor_second", 9)
  object major_second extends Interval(2,0,"major_second", 8)
  object minor_third extends Interval( 3,0,"minor_third", 5)
  object major_third extends Interval(4,0,"major_third", 4)
  object perfect_fourth extends Interval(5,0,"perfect_fourth", 2)
  object augmented_fourth_or_diminished_fifth extends Interval(6,0,"augmented_fourth_or_diminished_fifth", 11)
  object perfect_fifth extends Interval(7,0,"perfect_fifth", 1)
  object minor_sixth extends Interval(8,0,"minor_sixth", 6)
  object major_sixth extends Interval(9,0,"major_sixth", 3)
  object minor_seventh extends Interval(10,0,"minor_seventh", 7)
  object major_seventh extends Interval(11,0, "major_seventh", 10)

  val intervals = List(unison, minor_second, major_second, minor_third, major_third, perfect_fourth, augmented_fourth_or_diminished_fifth, perfect_fifth, minor_sixth, major_sixth, minor_seventh, major_seventh)
    .zipWithIndex.map{case (interval, index) => (index.asInstanceOf[ScaleDegree], interval)}.toMap

}
