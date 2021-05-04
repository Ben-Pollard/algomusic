package models

import models.Interval.intervals
import models.Primitives.{MidiPitch, ScaleDegree}

case class Chord(scaleDegrees: List[ScaleDegree], scale: Scale) {

  case class ChordInfo(voicing: List[ScaleDegree], dissonance: Int, intervalNames: Set[String], avgDegree: Float, unevenness: Float, dissonanceOfLargestInterval: Int, range: Int)

  def leading(chord: Chord): Chord = {
    //modify this chord to reduce dissonance with passed chord

    val permutations = generatePermutations(false, true, numVoices = scaleDegrees.size)
      .map(p => (p.voicing, pitches(Chord(p.voicing, scale)).toSet))


    val nextChordPitches = pitches(chord).toSet
    //get pitches of permutations and unite them with the next chord
    //get least discordant resulting chord
    val nearestVoicing = permutations
      .map(p => {
      val chordUnion = p._2.union(nextChordPitches)
      val dissonance = getIntervals(chordUnion).map(i => i.dissonanceRank).sum
      val difference = p._2.zip(nextChordPitches).map{case (a,b) => (a-b).abs}.sum
      (p, dissonance, difference)
    })
      .sortBy(permutation =>  (permutation._3, permutation._2)).head._1._1

    Chord(nearestVoicing, scale)
  }

  def voicing(staticRoot: Boolean, allowInversions: Boolean, numVoices: Int = scaleDegrees.size): Chord = {

    val degreesOfNVoices = scaleDegrees :+ scaleDegrees.head + 7

    val voicingInfo = generatePermutations(staticRoot, allowInversions, numVoices = degreesOfNVoices.size)

    val avgDegree = degreesOfNVoices.sum / degreesOfNVoices.size

    //todo maybe some normalised ranking here
    val consonant = voicingInfo.sortBy(v => (v.dissonance, v.unevenness, (v.avgDegree - avgDegree).abs)).head.voicing
    val internallySpicy = voicingInfo.sortBy(v => (v.dissonanceOfLargestInterval, -v.dissonance, (v.avgDegree - avgDegree).abs)).head.voicing
    val spicy = voicingInfo.sortBy(v => (v.dissonance, -(v.avgDegree - avgDegree).abs)).last.voicing
    val spicyOpen = voicingInfo.sortBy(v => (v.dissonance, v.range, -(v.avgDegree - avgDegree).abs)).last.voicing
    val closedEven = voicingInfo.sortBy(v => (v.range, v.unevenness, (v.avgDegree - avgDegree).abs)).head.voicing
    val openEven = voicingInfo.sortBy(v => (v.unevenness, -v.range, (v.avgDegree - avgDegree).abs)).head.voicing

    Chord(spicyOpen , scale)
  }

  private def generatePermutations(staticRoot: Boolean, allowInversions: Boolean, numVoices: Int = scaleDegrees.size) = {
    val permutationSeed = Vector(-7,0,7)

    val degreesOfNVoices = scaleDegrees :+ scaleDegrees.head + 7 //todo check addition is appropriate. how to allow for adding in alternative positions?

    val numPermutations = math.pow(permutationSeed.size, degreesOfNVoices.size).toInt

    val permutations = (0 until numPermutations)
      .map(n => {
        val rebasedNum = s"%0${degreesOfNVoices.size}d".format(Integer.toString(n, permutationSeed.size).toInt)
        val permutation = rebasedNum.map(c => permutationSeed(c.asDigit))
        val degrees = permutation zip degreesOfNVoices map(pc => pc._1 + pc._2)
        degrees.toSet
      }).toList

    val allowablePermutations = ((staticRoot, allowInversions) match {
      case (_, false) => permutations.filter(p => p.head == degreesOfNVoices.head && p.min >= degreesOfNVoices.head)
      case (true, true) => permutations.filter(_.head == degreesOfNVoices.head)
      case _ => permutations
    }).filter(_.size == numVoices)

    val voicingInfo = allowablePermutations.map(p => getInfo(Chord(p.toList, scale)))

    voicingInfo
  }

  private def pitches(chord: Chord) = {
    chord.scaleDegrees.map(d => chord.scale.degreeMap(d))
  }

  private def getIntervals(pitches: Set[MidiPitch]) = {
    (for {
      (i, ix) <- pitches.zipWithIndex
      (j, jx) <- pitches.zipWithIndex
      if (ix < jx)
    } yield (i - j).abs)
      .map(interval => intervals.get(interval % 12).get)
  }

  private def getInfo(chord: Chord): ChordInfo = {
    val pitches = chord.scaleDegrees.map(d => chord.scale.degreeMap(d)).toSet

    //all pitch pairs in the chord permutation
    val intervalsInChord = getIntervals(pitches)

    val dissonance = intervalsInChord.map(i => i.dissonanceRank).sum
    val names = intervalsInChord.map(_.name)
    val consecutiveDegreeDiffs = chord.scaleDegrees.toList.sorted.sliding(2).map { case Seq(x, y, _*) => y - x }
    val unevenness = consecutiveDegreeDiffs.toSet.sum.toFloat / (chord.scaleDegrees.max - chord.scaleDegrees.min)
    val dissonanceOfLargestInterval = intervalsInChord.toList.sortBy(_.tone).last.dissonanceRank

    ChordInfo(
      voicing = chord.scaleDegrees.toList,
      dissonance = dissonance,
      intervalNames = names.toSet,
      avgDegree = chord.scaleDegrees.sum.toFloat / chord.scaleDegrees.size.toFloat,
      unevenness = unevenness,
      dissonanceOfLargestInterval = dissonanceOfLargestInterval,
      range = chord.scaleDegrees.max - chord.scaleDegrees.min)
  }
}

