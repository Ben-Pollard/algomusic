package transformers

import models.Primitives.{Duration, Velocity}
import models.{AbstractRhythm, Rhythm}
import util.{SequenceTransformers, Util}
import util.Util.{idxTuple2SubIdx, subIdx2Tuple}

trait RhythmTransformers extends AbstractRhythm {
  def setVelocities(newVelocities: Seq[Velocity]): Rhythm = {
    Rhythm(beats, subdivisions, hitIndices, hitDurations, newVelocities)
  }

  def setDurations(newDurations: Seq[Duration]): Rhythm = {
    Rhythm(beats, subdivisions, hitIndices, newDurations, velocities)
  }
  def setDurations(newDurations: Seq[Duration], newHitDurations: Seq[Duration]): Rhythm = {
    Rhythm(beats, subdivisions, hitIndices, newDurations, velocities)
  }



  def dynamics(min: Velocity, max: Velocity): Rhythm = {
    val inputRange: Float = velocities.max - velocities.min
    val outputRange: Velocity = max - min
    val newVelocities = velocities.map(v => {
      val pct = (v - velocities.min) / inputRange
      ((pct * outputRange) + min).round
    })
    setVelocities(newVelocities)

  }

  def take(n: Int): Rhythm = {
    Rhythm(beats, subdivisions, hitIndices.take(n), hitDurations.take(n), velocities.take(n))
  }

  def expandDurations(): Rhythm = {
    val newDurations = (Left(0.0) +: durations).zip(durations :+ Left(0.0))
      .map(d => if (d._1.isLeft) Left(d._1.left.get + d._2.right.get) else Right(0.0))
      .tail

    val hitDurations = newDurations.filter(_.isLeft).map(_.left.get)
    setDurations(hitDurations)
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

  def rotate(wholeSteps: Int): Rhythm = {
    rotate(wholeSteps, 0)
  }

  def rotateVelocities(n: Int) : Rhythm= {
    Rhythm(beats, subdivisions, hitIndices, hitDurations, SequenceTransformers.rotate(velocities, n))
  }

  def multiply(rhythm: Rhythm, factor: Double): Rhythm = {
    val extendedDurations = rhythm.durations.map(d => if (d.isLeft) Left(d.left.get * factor) else Right(d.right.get * factor))
    Rhythm(rhythm.beats, rhythm.subdivisions, rhythm.hitIndices, rhythm.hitDurations.map(d => d*factor), extendedDurations, rhythm.velocities)
  }

  def reverse(rhythm: Rhythm): Rhythm = {
    val reversedIndices = subIdx2Tuple(idxTuple2SubIdx(rhythm.hitIndices, rhythm.subdivisions).reverse, rhythm.subdivisions)
    Rhythm(rhythm.beats, rhythm.subdivisions, reversedIndices, rhythm.hitDurations.reverse, rhythm.durations.reverse, rhythm.velocities.reverse)
  }

  def swing(rhythm: Rhythm, subdivisionMultiple: Int): Rhythm = {
    //move every other hit forward
    val newSubdivisions = rhythm.subdivisions * subdivisionMultiple
    val newSteps = rhythm.beats * newSubdivisions
    val addedSteps = rhythm.expandSubdivisions(newSubdivisions)
    val swungIndices = subIdx2Tuple(idxTuple2SubIdx(addedSteps.hitIndices, newSubdivisions).map(i => (if ((i / subdivisionMultiple) % 2==0) i else i+1) % newSteps), newSubdivisions)
    Rhythm(rhythm.beats, newSubdivisions, swungIndices, rhythm.hitDurations, rhythm.velocities)
  }

  //  def shift(rhythm: Rhythm, wholeSteps: Int, subdivisions: Int): Rhythm = {
  //    //break down into an equivalent whole-step rhythm, then shift, then re-pack into subdivisions
  //    val indicesIntoSubdivisions = rhythm.hitIndices.map(i => i._1*rhythm.subdivisions + i._2)
  //    val subdivsToShift = wholeSteps * rhythm.subdivisions + subdivisions
  //    val newSubdivIndices = indicesIntoSubdivisions.map(i => (i + subdivsToShift) % (rhythm.beats * rhythm.subdivisions))
  //    val sortedHitdurations = rhythm.hitDurations zip newSubdivIndices  sortBy(_._2) map(_._1)
  //    val newHitindices = newSubdivIndices.sorted.map(i => (i/rhythm.subdivisions, i % rhythm.subdivisions))
  //    Rhythm(rhythm.beats, rhythm.subdivisions, newHitindices, sortedHitdurations)
  //  }
  //
  //  def shift(rhythm: Rhythm, wholeSteps: Int): Rhythm = {
  //    shift(rhythm, wholeSteps, 0)
  //  }

  def subtract(rhythm: Rhythm): Rhythm = {
    assert(beats == rhythm.beats)
    val lcm = Util.lowestCommonMultiple(List(subdivisions, rhythm.subdivisions))
    val thisLcm = this.expandSubdivisions(lcm)
    val thatLcmIndices = rhythm.expandSubdivisions(lcm).hitIndices.toSet
    val matches = thisLcm.hitIndices.map(i => thatLcmIndices.contains(i))
    Rhythm(
      beats,
      lcm,
      (thisLcm.hitIndices zip matches).filterNot(_._2).map(_._1),
      (thisLcm.hitDurations zip matches).filterNot(_._2).map(_._1),
      (thisLcm.velocities zip matches).filterNot(_._2).map(_._1)
    )
      .compressSubdivisions(subdivisions)
  }

  def setBeatsPerBar(newBeats: Int): Rhythm = {
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

  def expandSubdivisions(newSubdivisions: Int) = {
    val newStepCount = newSubdivisions * beats
    assert(newStepCount % (beats * subdivisions) == 0)
    val flattenedIndices = idxTuple2SubIdx(hitIndices, subdivisions)
    val newIndices = subIdx2Tuple(flattenedIndices.map(i => i * newSubdivisions / subdivisions), newSubdivisions)
    Rhythm(beats, newSubdivisions, newIndices, hitDurations, velocities)
  }

  def compressSubdivisions(newSubdivisions: Int) = {
    val keep1InN = subdivisions / newSubdivisions
    val flattenedIndices = idxTuple2SubIdx(hitIndices, subdivisions)
    val matches = flattenedIndices.map(_%keep1InN==0)
    val filteredIndices = flattenedIndices.zip(matches).filter(_._2).map(_._1 / keep1InN)
    val newHitIndices = subIdx2Tuple(filteredIndices, newSubdivisions)
    Rhythm(beats, newSubdivisions, newHitIndices, hitDurations, velocities)
  }
}
