package models

import models.Primitives.{Duration, RestDuration, RhythmDurations, s}

//the job of this class is to carry the note/rest durations for the Midi Sequencer, along with information used by the transformers
case class Rhythm(beats: Int, subdivisions: Int, hitIndices: Seq[(Int,Int)], hitDurations: Seq[Duration], durations: RhythmDurations) {

  def take(n: Int): Rhythm = {
    Rhythm(beats, subdivisions, hitIndices.take(n), hitDurations.take(n))
  }

  def info() = {
    println(s"Steps: ${beats}; Subdivisions: ${subdivisions}, Hits: ${hitIndices}")
  }

}

object Rhythm {

  //durations are expressed in beats
  //steps are whole, not whole*subdivisions
  //hit indices are expressed as indexes into beats, then into subdivisions


  //constructor for rhythms with 1 beat
  def apply(steps:Int, hitIndices: Seq[Int], hitDurations: Seq[Duration]): Rhythm = {
    apply(1, steps, hitIndices.map(i => (0,i)), hitDurations)
  }

  //base constructor to yield durations with rests
  def apply(beats:Int, subdivisions: Int, hitIndices: Seq[(Int,Int)], hitDurations: Seq[Duration]): Rhythm = {
    assert(hitIndices.map(_._1).max <= beats - 1)
    assert(hitIndices.map(_._2).max < subdivisions)
    assert(hitIndices.length == hitDurations.length)

    val totalSubdivisions = beats * subdivisions

    val indicesIntoSubdivisions = hitIndices.map(i => i._1*subdivisions + i._2)

    val subDivisionLen:Duration = beats / totalSubdivisions.toDouble

    val nonOverlapHitDurations: Seq[Duration] = (0 until indicesIntoSubdivisions.length -1 map(i => {
      Vector((indicesIntoSubdivisions(i+1) - indicesIntoSubdivisions(i)) * subDivisionLen, hitDurations(i)).min
    })) :+ Vector((totalSubdivisions - indicesIntoSubdivisions.last) * subDivisionLen, hitDurations.last).min

    val restDurations: Seq[RestDuration] = ((indicesIntoSubdivisions.head * subDivisionLen) +: ((0 until indicesIntoSubdivisions.length -1) map(i => {
      ((indicesIntoSubdivisions(i+1) - indicesIntoSubdivisions(i)) * subDivisionLen) - nonOverlapHitDurations(i)
    })) :+ ((totalSubdivisions - indicesIntoSubdivisions.last) * subDivisionLen) - nonOverlapHitDurations.last)

    assert("%.12f".format(nonOverlapHitDurations.sum + restDurations.sum).toDouble == beats.toDouble)

    val nonOverlapNoteDurationsIt = nonOverlapHitDurations.toIterator
    val restDurationsIt = restDurations.toIterator

    val durations = 1 to (hitIndices.length*2 + 1) map(b => if(b % 2 == 0) Left(nonOverlapNoteDurationsIt.next()) else Right(restDurationsIt.next()))

    new Rhythm(beats, subdivisions, hitIndices, hitDurations, durations)
  }

}
