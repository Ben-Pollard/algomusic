package models

import models.Interval.intervals
import models.Primitives.ScaleDegree

case class Chord(scaleDegrees: List[ScaleDegree], scale: Scale) {

  case class ChordInfo(voicing: List[ScaleDegree], dissonance: Int, intervalNames: Set[String], avgDegree: Float, evenness: Int, dissonanceOfLargestInterval: Int, range: Int)

  def voicing(staticRoot: Boolean, allowInversions: Boolean): Chord = {
    val permutationSeed = Vector(-7,0,7)

    val numPermutations = math.pow(permutationSeed.size, scaleDegrees.size).toInt

    val permutations = (0 until numPermutations)
      .map(n => {
        val rebasedNum = s"%0${scaleDegrees.size}d".format(Integer.toString(n, permutationSeed.size).toInt)
        val permutation = rebasedNum.map(c => permutationSeed(c.asDigit))
        val degrees = permutation zip scaleDegrees map(pc => pc._1 + pc._2)
        degrees.toSet
      }).toList

    val allowablePermutations = (staticRoot, allowInversions) match {
      case (_, false) => permutations.filter(p => p.head == scaleDegrees.head && p.min >= scaleDegrees.head)
      case (true, true) => permutations.filter(_.head == scaleDegrees.head)
      case _ => permutations

    }

    val voicingInfo = allowablePermutations.map(p => {
      val pitches = p.map(d => scale.degreeMap(d))

      //all pitch pairs in the chord permutation
      val intervalsInChord = (for {
        (i, ix) <- pitches.zipWithIndex
        (j, jx) <- pitches.zipWithIndex
        if (ix < jx)
      } yield (i - j).abs)
        .map(interval => intervals.get(interval % 12).get)

      val dissonance = intervalsInChord.map(i => i.dissonanceRank).sum
      val names = intervalsInChord.map(_.name)
      val consecutiveDegreeDiffs = p.toList.sorted.sliding(2).map { case Seq(x, y, _*) => y - x }
      val evenness = consecutiveDegreeDiffs.toSet.size
      val dissonanceOfLargestInterval = intervalsInChord.toList.sortBy(_.tone).last.dissonanceRank

      ChordInfo(p.toList, dissonance, names, p.sum.toFloat / p.size.toFloat, evenness, dissonanceOfLargestInterval, p.max - p.min)
    })

    val avgDegree = scaleDegrees.sum / scaleDegrees.size

    val mostConsonant = voicingInfo.sortBy(v => (v.dissonance, (v.avgDegree - avgDegree).abs)).head.voicing
    val internallySpicy = voicingInfo.sortBy(v => (v.dissonanceOfLargestInterval, -v.dissonance)).head.voicing
    val spiciest = voicingInfo.sortBy(v => (v.dissonance, -(v.avgDegree - avgDegree).abs)).last.voicing
    val mostEven = voicingInfo.sortBy(v => (v.evenness, (v.avgDegree - avgDegree).abs)).head.voicing
    val mostOpen = voicingInfo.sortBy(v => (v.evenness, -v.range)).head.voicing

    Chord(mostOpen, scale)
  }
}

