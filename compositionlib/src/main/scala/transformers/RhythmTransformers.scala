package transformers

import models.Primitives.Duration
import models.Rhythm


object RhythmTransformers {


  def idxTuple2SubIdx(idx: Seq[(Int, Int)], subdivisions: Int) = {
    idx.map(i => i._1*subdivisions + i._2)
  }

  def subIdx2Tuple(subidx: Seq[Int], subdivisions: Int) = {
    subidx.map(s => (s/subdivisions, s%subdivisions))
  }

  def multiply(rhythm: Rhythm, factor: Double): Rhythm = {
    val extendedDurations = rhythm.durations.map(d => if (d.isLeft) Left(d.left.get * factor) else Right(d.right.get * factor))
    Rhythm(rhythm.beats, rhythm.subdivisions, rhythm.hitIndices, rhythm.hitDurations.map(d => d*factor), extendedDurations)
  }

  def reverse(rhythm: Rhythm): Rhythm = {
    val reversedIndices = subIdx2Tuple(idxTuple2SubIdx(rhythm.hitIndices, rhythm.subdivisions).reverse, rhythm.subdivisions)
    Rhythm(rhythm.beats, rhythm.subdivisions, reversedIndices, rhythm.hitDurations.reverse, rhythm.durations.reverse)
  }

  def swing(rhythm: Rhythm, subdivisionMultiple: Int): Rhythm = {
    //move every other hit forward
    val newSubdivisions = rhythm.subdivisions * subdivisionMultiple
    val newSteps = rhythm.beats * newSubdivisions
    val addedSteps = addSubdivisions(rhythm, newSubdivisions)
    val swungIndices = subIdx2Tuple(idxTuple2SubIdx(addedSteps.hitIndices, newSubdivisions).map(i => (if ((i / subdivisionMultiple) % 2==0) i else i+1) % newSteps), newSubdivisions)
    Rhythm(rhythm.beats, newSubdivisions, swungIndices, rhythm.hitDurations)
  }

  def subtractFromFilled(rhythm: Rhythm): Rhythm = {
    val flatIndices = idxTuple2SubIdx(rhythm.hitIndices, rhythm.subdivisions).toSet
    val invertedIndices = subIdx2Tuple((0 until rhythm.beats * rhythm.subdivisions).filter(i => !(flatIndices contains i)), rhythm.subdivisions)
    val hitDurations = invertedIndices.map(i => 1.0 / rhythm.subdivisions).asInstanceOf[Seq[Duration]]
    Rhythm(rhythm.beats, rhythm.subdivisions, invertedIndices, hitDurations)
  }

  def shift(rhythm: Rhythm, wholeSteps: Int, subdivisions: Int): Rhythm = {
    //break down into an equivalent whole-step rhythm, then shift, then re-pack into subdivisions
    val indicesIntoSubdivisions = rhythm.hitIndices.map(i => i._1*rhythm.subdivisions + i._2)
    val subdivsToShift = wholeSteps * rhythm.subdivisions + subdivisions
    val newSubdivIndices = indicesIntoSubdivisions.map(i => (i + subdivsToShift) % (rhythm.beats * rhythm.subdivisions))
    val sortedHitdurations = rhythm.hitDurations zip newSubdivIndices  sortBy(_._2) map(_._1)
    val newHitindices = newSubdivIndices.sorted.map(i => (i/rhythm.subdivisions, i % rhythm.subdivisions))
    Rhythm(rhythm.beats, rhythm.subdivisions, newHitindices, sortedHitdurations)
  }

  def shift(rhythm: Rhythm, wholeSteps: Int): Rhythm = {
    shift(rhythm, wholeSteps, 0)
  }

  //changes the number of subdivisions
  def steps2Subdivisions(rhythm: Rhythm, newSubdivisions: Int): Rhythm = {
    //flatten into 1 beat with n subdivs
    val flattenedIndices = idxTuple2SubIdx(rhythm.hitIndices, rhythm.subdivisions)
    val totalSubdivisions = rhythm.beats * rhythm.subdivisions
    //check total subdivs is a factor of newdubdivs
    assert(totalSubdivisions % (rhythm.beats * newSubdivisions) == 0)
    //group into beats
    val newBeats = totalSubdivisions / newSubdivisions
    val newIndices = flattenedIndices.map(i => (i / newSubdivisions, i % newSubdivisions))
    Rhythm(newBeats, newSubdivisions, newIndices, rhythm.hitDurations)
  }

  def addSubdivisions(rhythm: Rhythm, newSubdivisions: Int) = {
    val newStepCount = newSubdivisions * rhythm.beats
    assert(newStepCount % (rhythm.beats * rhythm.subdivisions) == 0)
    val flattenedIndices = idxTuple2SubIdx(rhythm.hitIndices, rhythm.subdivisions)
    val newIndices = subIdx2Tuple(flattenedIndices.map(i => i * newSubdivisions / rhythm.subdivisions), newSubdivisions)
    Rhythm(rhythm.beats, newSubdivisions, newIndices, rhythm.hitDurations)
  }

}
