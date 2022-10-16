package models

import models.Primitives.{Duration, RestDuration, RhythmDurations, Velocity}
import transformers.RhythmTransformers


//the job of this class is to carry the note/rest durations for the Midi Sequencer, along with information used by the transformers
abstract class AbstractRhythm(
                               val beats: Int,
                               val subdivisions: Int,
                               val hitIndices: Seq[(Int,Int)],
                               val hitDurations: Seq[Duration],
                               val durations: RhythmDurations,
                               val velocities: Seq[Velocity])

case class Rhythm (
                    override val beats: Int,
                    override val subdivisions: Int,
                    override val hitIndices: Seq[(Int,Int)],
                    override val hitDurations: Seq[Duration],
                    override val durations: RhythmDurations,
                    override val velocities: Seq[Velocity])
  extends AbstractRhythm (
    beats: Int,
    subdivisions: Int,
    hitIndices: Seq[(Int,Int)],
    hitDurations: Seq[Duration],
    durations: RhythmDurations,
    velocities: Seq[Velocity])

    with RhythmTransformers {


  def info() = {
    println(s"Steps: ${beats}; Subdivisions: ${subdivisions}, Hits: ${hitIndices}")
  }

}

object Rhythm {

  //durations are expressed in beats
  //steps are whole, not whole*subdivisions
  //hit indices are expressed as indexes into beats, then into subdivisions


  //constructor for rhythms with 1 beat
  def apply(steps:Int, hitIndices: Seq[Int], hitDurations: Seq[Duration], velocities: Seq[Velocity]): Rhythm = {
    apply(1, steps, hitIndices.map(i => (0,i)), hitDurations, velocities)
  }

  //base constructor to yield durations with rests
  def apply(beats:Int, subdivisions: Int, hitIndices: Seq[(Int,Int)], hitDurations: Seq[Duration], velocities: Seq[Velocity]): Rhythm = {
    if (hitIndices.length == 0) {
      return new Rhythm(beats, subdivisions, hitIndices, hitDurations, Seq(), velocities)
    }

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

    new Rhythm(beats, subdivisions, hitIndices, hitDurations, durations, velocities)
  }

}
