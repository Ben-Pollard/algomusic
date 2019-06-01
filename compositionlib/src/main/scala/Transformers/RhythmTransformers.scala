package Transformers

import models.Primitives.Rhythm


object RhythmTransformers {

  def rotate[A](seq: Seq[A], i: Int) = {
    val size = seq.size
    val (first, last) = seq.splitAt(size - (i % size))
    last ++ first
  }

  def multiply(rhythm: Rhythm, factor: Double): Rhythm = {
    val extendedDurations = rhythm.durations.map(d => if (d.isLeft) Left(d.left.get * factor) else Right(d.right.get * factor))
    Rhythm(rhythm.steps, rhythm.subdivisions, rhythm.hitIndices, rhythm.hitDurations.map(d => d*factor), extendedDurations)
  }

  def shift(rhythm: Rhythm, wholeSteps: Int, subdivisions: Int): Rhythm = {
    //break down into an equivalent whole-step rhythm, then shift, then re-pack into subdivisions
    val indicesIntoSubdivisions = rhythm.hitIndices.map(i => i._1*rhythm.subdivisions + i._2)
    val subdivsToShift = wholeSteps * rhythm.subdivisions + subdivisions
    val newSubdivIndices = indicesIntoSubdivisions.map(i => (i + subdivsToShift) % (rhythm.steps * rhythm.subdivisions))
    val sortedHitdurations = rhythm.hitDurations zip newSubdivIndices  sortBy(_._2) map(_._1)
    val newHitindices = newSubdivIndices.sorted.map(i => (i/rhythm.subdivisions, i % rhythm.subdivisions))
    Rhythm(rhythm.steps, rhythm.subdivisions, newHitindices, sortedHitdurations)
  }

  def shift(rhythm: Rhythm, wholeSteps: Int): Rhythm = {
    shift(rhythm, wholeSteps, 0)
  }

  //changes the number of subdivisions
  //if new subdivisions is a factor of old, rhythm will sound the same, otherwise not
  def modifySubdivisions(rhythm: Rhythm, newSubdivisions: Int): Rhythm = {
    val newIndices = rhythm.hitIndices.map(i => (i._1, i._2 * newSubdivisions / rhythm.subdivisions))
    Rhythm(rhythm.steps, newSubdivisions, newIndices, rhythm.hitDurations)
  }

}
