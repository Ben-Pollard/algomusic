package generators

import models.Primitives.{Duration, durations, w}
import models._
import util.Util.subIdx2Tuple



object Meter {

  def allSubdivisions(beats: Int, subdivisions: Int) = {
    val steps = beats * subdivisions
    val hitIndices = subIdx2Tuple(0 until steps toList, subdivisions)

    //max duration is the max duration that is no longer than the subdivision.
    //For very small subdivisions there may be no standard duration that is smaller than the minimum defined note duration
    val subdivDuration = 1.0 / subdivisions
    val maxDuration = durations.filter(_ <= subdivDuration).max
    val hitDurations: Seq[Duration] = hitIndices.map(i => maxDuration)
    val velocities = List.fill(hitDurations.length)(100)

    Rhythm(beats, subdivisions, hitIndices, hitDurations, velocities)
  }

  def onBeats(beats: Int, subdivisions: Int) = {
    val steps = beats * subdivisions
    //hit on the beat, every subdivisions steps
    val hitIndices = subIdx2Tuple(0 until steps by subdivisions toList, subdivisions)

    //max duration is 1 beat
    val maxDuration = w
    val hitDurations: Seq[Duration] = hitIndices.map(i => maxDuration)
    val velocities = List.fill(hitDurations.length)(100)

    Rhythm(beats, subdivisions, hitIndices, hitDurations, velocities)
  }

  def theOne(beats: Int, subdivisions: Int) = {
    val steps = beats * subdivisions
    //hit on the beat, every subdivisions steps
    val hitIndices = List((0,0))

    //max duration is 1 beat
    val maxDuration = w
    val hitDurations: Seq[Duration] = List(maxDuration)
    val velocity = List(100)

    Rhythm(beats, subdivisions, hitIndices, hitDurations, velocity)
  }

  def beatIndices(beats: Int, subdivisions: Int, indices: List[Int]) = {
    val steps = beats * subdivisions
    val hitIndices = indices.map(i => (i, 0))
    //max duration is 1 beat
    val maxDuration = w
    val hitDurations: Seq[Duration] = hitIndices.map(i => maxDuration)
    val velocities = List.fill(hitDurations.length)(100)
    Rhythm(beats, subdivisions, hitIndices, hitDurations, velocities)
  }
}
