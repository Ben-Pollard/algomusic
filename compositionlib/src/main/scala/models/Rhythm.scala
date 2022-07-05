package models

import models.Primitives.{Duration, RestDuration, RhythmDurations, Velocity}
import transformers.RhythmTransformers.{idxTuple2SubIdx, subIdx2Tuple}
import transformers.SequenceTransformers

//the job of this class is to carry the note/rest durations for the Midi Sequencer, along with information used by the transformers
case class Rhythm(beats: Int, subdivisions: Int, hitIndices: Seq[(Int,Int)], hitDurations: Seq[Duration], durations: RhythmDurations, velocities: Seq[Velocity]) {

  def setVelocities(newVelocities: Seq[Velocity]): Rhythm = {
    this.copy(velocities = newVelocities)
  }

  def setDurations(newDurations: Seq[Duration]): Rhythm = {
    Rhythm(beats, subdivisions, hitIndices, newDurations, velocities)
  }

  def dynamics(min: Velocity, max: Velocity): Rhythm = {
    val inputRange: Float = velocities.max - velocities.min
    val outputRange: Velocity = max - min
    this.copy(velocities = velocities.map(v => {
      val pct = (v - velocities.min) / inputRange
      ((pct * outputRange) + min).round
    }))
  }

  def take(n: Int): Rhythm = {
    Rhythm(beats, subdivisions, hitIndices.take(n), hitDurations.take(n), velocities.take(n))
  }

  def info() = {
    println(s"Steps: ${beats}; Subdivisions: ${subdivisions}, Hits: ${hitIndices}")
  }

  def rotate(wholeSteps: Int, subdivs: Int): Rhythm = {
    //break down into an equivalent whole-step rhythm, then shift, then re-pack into subdivisions
    val indicesIntoSubdivisions = hitIndices.map(i => i._1*subdivisions + i._2)
    val subdivsToShift = wholeSteps * subdivisions + subdivs
    val newSubdivIndices = indicesIntoSubdivisions.map(i => (i + subdivsToShift) % (beats * subdivisions))
    val sortedHitdurations = hitDurations zip newSubdivIndices  sortBy(_._2) map(_._1)
    val sortedVelocities = velocities zip newSubdivIndices  sortBy(_._2) map(_._1)
    val newHitindices = newSubdivIndices.sorted.map(i => (i/subdivisions, i % subdivisions))
    Rhythm(beats, subdivisions, newHitindices, sortedHitdurations, sortedVelocities)
  }

  def rotateVelocities(n: Int) : Rhythm= {
    Rhythm(beats, subdivisions, hitIndices, hitDurations, SequenceTransformers.rotate(velocities, n))
  }

  def beatsPerBar(newBeats: Int): Rhythm = {
    val totalSubdivisions = beats * subdivisions
    assert(totalSubdivisions % newBeats == 0)
    val flattenedIndices = idxTuple2SubIdx(hitIndices, subdivisions)
    val newSubdivisions = totalSubdivisions / newBeats
    val newIndices = flattenedIndices.map(i => (i/newSubdivisions, i%newSubdivisions))
    Rhythm(newBeats, newSubdivisions, newIndices, hitDurations, velocities)
  }

  //how to handle overlap? shorten or combine?
  def +(rhythm: Rhythm): Rhythm = {
    val totalSubdivisions1 = beats * subdivisions
    val totalSubdivisions2 = rhythm.beats * rhythm.subdivisions
    assert(totalSubdivisions1 == totalSubdivisions2)

    val idx1Dur = idxTuple2SubIdx(hitIndices, subdivisions) zip hitDurations toMap
    val idx2Dur = idxTuple2SubIdx(rhythm.hitIndices, rhythm.subdivisions) zip( rhythm.hitDurations) toMap
    val idx1Vel = idxTuple2SubIdx(hitIndices, subdivisions) zip velocities toMap
    val idx2Vel = idxTuple2SubIdx(rhythm.hitIndices, rhythm.subdivisions) zip( rhythm.velocities) toMap

    val newFlatIndices = (0 until totalSubdivisions1 toList).filter(i => (idx1Dur.contains(i) || idx2Dur.contains(i)))

    val combinedIndices = subIdx2Tuple(newFlatIndices, subdivisions)
    val combinedDurations = newFlatIndices.map(i => List(idx1Dur.get(i), idx2Dur.get(i)).flatten.max)
    val combinedVelocities = newFlatIndices.map(i => List(idx1Vel.get(i), idx2Vel.get(i)).flatten.max)

    Rhythm(beats, subdivisions, combinedIndices, combinedDurations, combinedVelocities)
  }

  //changes the number of subdivisions
  def steps2Subdivisions(newSubdivisions: Int): Rhythm = {
    //flatten into 1 beat with n subdivs
    val flattenedIndices = idxTuple2SubIdx(hitIndices, subdivisions)
    val totalSubdivisions = beats * subdivisions
    //check total subdivs is a factor of newdubdivs
//    assert(totalSubdivisions % (beats * newSubdivisions) == 0)
    //group into beats
    val newBeats = totalSubdivisions / newSubdivisions
    val newIndices = flattenedIndices.map(i => (i / newSubdivisions, i % newSubdivisions))
    Rhythm(newBeats, newSubdivisions, newIndices, hitDurations, velocities)
  }

  def addSubdivisions(newSubdivisions: Int) = {
    val newStepCount = newSubdivisions * beats
    assert(newStepCount % (beats * subdivisions) == 0)
    val flattenedIndices = idxTuple2SubIdx(hitIndices, subdivisions)
    val newIndices = subIdx2Tuple(flattenedIndices.map(i => i * newSubdivisions / subdivisions), newSubdivisions)
    this.copy(subdivisions = newSubdivisions, hitIndices = newIndices)
  }

  def rotate(wholeSteps: Int): Rhythm = {
    rotate(wholeSteps, 0)
  }

  private def idxTuple2SubIdx(idx: Seq[(Int, Int)], subdivisions: Int) = {
    idx.map(i => i._1*subdivisions + i._2)
  }

  private def subIdx2Tuple(subidx: Seq[Int], subdivisions: Int) = {
    subidx.map(s => (s/subdivisions, s%subdivisions))
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
