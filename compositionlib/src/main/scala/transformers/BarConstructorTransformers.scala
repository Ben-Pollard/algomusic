package transformers

import enums.Interval.intervals
import models.Primitives.ScaleDegree
import models.{PolyphonicScalePhrase, Rhythm, ScalePhrase, barconstructors}
import models.barconstructors.{AbstractScalePhraseBarConstructor, PolyphonicScalePhraseBarConstructor, ScalePhraseBarConstructor}

trait BarConstructorTransformers extends AbstractScalePhraseBarConstructor {

  def toPoly(): PolyphonicScalePhraseBarConstructor = {
    barconstructors.PolyphonicScalePhraseBarConstructor(PolyphonicScalePhrase(List(scalePhrase)), rhythm)
  }

  //create a scale phrase from an existing fragment
  def scalePhraseRunFiller(newRhythm: Rhythm, nextDegree: ScaleDegree): ScalePhraseBarConstructor = {
    //Things to think about with a melody:
    // Interval wrt to harmonic root & previous note
    // Motion wrt harmonic root & previous note
    // Placement of leaps
    // Rhythm
    // Use of tension
    // Setting the home note

    assert(rhythm.hitIndices.toSet.subsetOf(newRhythm.hitIndices.toSet))
    val extantContent = rhythm.hitIndices.zip(scalePhrase.degreeSequence).toMap

    val partialSequence = newRhythm.hitIndices.map(i => extantContent.get(i))
    val numStepsToFill = partialSequence.count(_.isEmpty)

    //try to step from start to end
    //Motion constrained to a range
    val firstAvailableDegree = extantContent.head._2

    //the range defines the melody as moving between 2 points - it doesn't have to
    val range = if (nextDegree > firstAvailableDegree) (firstAvailableDegree to nextDegree).toList else (nextDegree to firstAvailableDegree).reverse

    //      val scale = Scale(modeNumberMap.get(3).get, midiNoteNames.get("D").get)
    val pitches = range.map(d => scalePhrase.scale.degreeMap(d))

    //Info to help us pick our notes
    val stepIntervals = pitches.sliding(2).map { case Seq(x, y, _*) => (y - x).abs }.toList

    case class NoteInfo(degree: ScaleDegree, concordancyStart: Int, concordancyEnd: Int)

    val startPitch = scalePhrase.scale.degreeMap(range.head)
    val endPitch = scalePhrase.scale.degreeMap(range.last)

    val noteInfos = range.map(d => {
      val pitch = scalePhrase.scale.degreeMap(d)
      val startInterval = intervals.get((pitch - startPitch).abs % 12).get
      val endInterval = intervals.get((pitch - endPitch).abs % 12).get
      NoteInfo(d, startInterval.dissonanceRank, endInterval.dissonanceRank)
    })

    //pick a set of notes of size n
    val numberOfDegreesInRunRange = if (range.length > 2) range.length - 2 else range.length

    val mostConcordant = noteInfos
      .sortBy(n => n.concordancyStart * n.concordancyEnd)
      .filter(n => if (range.length > 2) n.concordancyEnd > 0 && n.concordancyStart > 0 else true)
      .take(numberOfDegreesInRunRange)
      .map(_.degree)

    //todo drop notes that are the same as the start and end if we can
    //repeat if we have to
    val newDegrees = partialSequence.head.get +: (if (mostConcordant.size >= numStepsToFill) {
      mostConcordant.take(numStepsToFill).sorted
    } else {
      (mostConcordant ++ List.fill(numStepsToFill - mostConcordant.size)(mostConcordant.head)).sorted
    })
    //      (0 until numStepsToFill).map(s => )
    //create a sequence of the correct length from the set and place them in some order


    //      val newDegrees = newRhythm.hitIndices.map(i => extantContent.getOrElse(i, nextDegree)).toList //just infills with the next note
    ScalePhraseBarConstructor(ScalePhrase(newDegrees.toList, scalePhrase.scale), newRhythm)
  }

}
