package transformers

import enums.Interval.{Interval, intervals}
import models.Primitives.{MidiPitch, ScaleDegree}
import models.{AbstractChord, Chord}
import enums.VoicingQualities.{CLOSED_EVEN, CONSONANT, INTERNALLY_SPICY, OPEN_EVEN, SPICY, SPICY_OPEN, VoicingQuality}
import util.Util.diatonicDegreeIsSameNoteLetter

import scala.collection.immutable

trait ChordTransformers extends AbstractChord {

  case class ChordInfo(voicing: List[ScaleDegree], dissonance: Int, intervalNames: List[String], avgDegree: Float, unevenness: Float, dissonanceOfLargestInterval: Int, range: Int)

  def leading(chord: Chord): Chord = {
    //modify this chord to reduce dissonance with chord passed as parameter

    // Generate permutations of the chord we want to modify
    val permutations = this.generatePermutations(true, this.scaleDegrees.toSet.size) //todo length should be this or chord?

    //for leading we always want the same number of voices.
    //due to repeats, the permutations may be less than the number of input voices
    // so filter for same num unique scale degrees between chords being compared, then repeat some to keep numVoices consistent.
    val permutationsWithSameNumUniqueDegrees = permutations
      .filter(p => p.scaleDegrees.toSet.size == this.scaleDegrees.toSet.size)
      .map(p => (p.scaleDegrees, p.getPitches().toSet))

    // Take the pitches of permutations of this and unite them with the pitches of permutations of chord.
    // Calculate dissonance between them
    val nextChordPitches = chord.getPitches().toSet
    //get pitches of subsets and unite them with the next chord
    //get least discordant resulting chord
    val nearestVoicing: List[ScaleDegree] = permutationsWithSameNumUniqueDegrees
      .map(p => {
        val (degreesOfPermutation: List[ScaleDegree], pitchesOfPermutation: Set[MidiPitch]) = p
        val chordUnion = pitchesOfPermutation.union(nextChordPitches)
        val dissonance = getIntervals(chordUnion).map(i => i.dissonanceRank).sum
        val difference = pitchesOfPermutation.toList.sorted.zip(nextChordPitches.toList.sorted).map{case (a,b) => (a-b).abs}.sum
        (p, dissonance, difference)
      })
      .sortBy(permutation =>  (permutation._3, permutation._2)).head._1._1

    val targetNumVoices = chord.scaleDegrees.length
    val additionalRequiredDegrees = targetNumVoices - nearestVoicing.length
    val nearestVoicingWithCorrectNumberofNotes = nearestVoicing ++ nearestVoicing.take(additionalRequiredDegrees)

    Chord(nearestVoicingWithCorrectNumberofNotes, scale)
  }

  def voicing(allowInversions: Boolean, voicingQuality: VoicingQuality, qualityRank: Int, numVoices: Int): Chord = {

    val startingDegrees = scaleDegrees :+ scaleDegrees.head + 7 //todo what is this doing?

    // this gives us all possible permutations of the scale where it's a re-voicing, up to a max of numVoices
    val voicingInfo: List[ChordInfo]= generatePermutations(allowInversions, numVoices)
      .map(permutation => getInfo(permutation)) //nb this is expensive. try to pre-filter

    val avgDegree = startingDegrees.sum.toFloat / startingDegrees.size.toFloat

    // these voicings may have less than the specified number of voices
    // always prefer the most available voices
    val rankedVoicings = voicingQuality match {
      case CONSONANT => voicingInfo.sortBy(v => (-v.voicing.size, v.dissonance, v.unevenness, (v.avgDegree - avgDegree).abs))
      case INTERNALLY_SPICY => voicingInfo.sortBy(v => (-v.voicing.size, v.dissonanceOfLargestInterval, -v.dissonance, (v.avgDegree - avgDegree).abs))
      case SPICY => voicingInfo.sortBy(v => (v.voicing.size, v.dissonance, -(v.avgDegree - avgDegree).abs)).reverse
      case SPICY_OPEN => voicingInfo.sortBy(v => (v.voicing.size, v.dissonance, v.range, -(v.avgDegree - avgDegree).abs)).reverse
      case CLOSED_EVEN => voicingInfo.sortBy(v => (-v.voicing.size, v.range, v.unevenness, (v.avgDegree - avgDegree).abs))
      case OPEN_EVEN => voicingInfo.sortBy(v => (-v.voicing.size, v.unevenness, -v.range, (v.avgDegree - avgDegree).abs))
    }

    // NB we need to return the number of voices requested so that a downstream List.transpose can work
    // This will create dupe notes. But they should fail to be added to the sequence by java..sequence.add()
    val chosenVoicing = rankedVoicings(qualityRank).voicing
    val additionalRequiredDegrees = numVoices - chosenVoicing.length
    val chosenVoicingWithCorrectNumberofNotes = chosenVoicing ++ chosenVoicing.take(additionalRequiredDegrees)

    Chord(chosenVoicingWithCorrectNumberofNotes, scale)
  }

  private def generatePermutations(allowInversions: Boolean, maxVoices: Int): List[Chord] = {

//    // debugging code to check note names
//    val noteNameMap = scale.getNoteNameMap().toMap
//    val noteNames = scaleDegrees.map(d => noteNameMap.get(d).get)
//    val uniqueLetters = noteNames.map(_.filter(!_.isDigit)).toSet
//    val allowableDegrees2ndAttempt = scale.degreeMap.keys.toList.sorted
//      .map(d => (d, noteNameMap(d)))
//      .filter(potential => uniqueLetters.contains(potential._2.filter(!_.isDigit)))

    val inputScaleDegreeModulos: Set[ScaleDegree] = scaleDegrees.map(d => (d+70)%7).toSet

    // subsets of any length. we may need to double up some notes to keep the specified numVoices
    val setOfAllowableDegrees = scale.degreeMap.keys.filter(potential => {
      scaleDegrees.exists(extantSetMember => diatonicDegreeIsSameNoteLetter(extantSetMember,potential))
    }).toSet

//    val subsets: List[Set[ScaleDegree]] = (inputScaleDegreeModulos.size to maxVoices).flatMap(setSize => {
//      // here we can check if max voices comes back with anything and generate smaller permutations only if not
//      setOfAllowableDegrees.toSet.subsets(setSize).toList
//    }).toList

    // iterate down from preferred subset size
    def getSubsets(subsetSize: Int, initSubset: List[Set[ScaleDegree]]): List[Set[ScaleDegree]] = {
      if (initSubset.isEmpty) {
        assert(subsetSize >= inputScaleDegreeModulos.size)
        val subset = setOfAllowableDegrees.subsets(subsetSize).toList
        getSubsets(subsetSize -1, subset)
      } else {
        initSubset
      }
    }
    val subsets: List[Set[ScaleDegree]] = getSubsets(maxVoices, List())

    val allowablesubsets: List[Set[ScaleDegree]] = (allowInversions match {
      case false => subsets.filter(subset => {
        diatonicDegreeIsSameNoteLetter(scaleDegrees.min, subset.min)
      })
      case true => subsets
    }).filter(subset => {
      // we're just re-voicing here. the note letters should be the same set as the original
      subset.map(d => (d+70)%7) == inputScaleDegreeModulos
    })

//    val voicingInfo = allowablesubsets.map(p => getInfo(Chord(p.toList, scale)))
//    voicingInfo

    allowablesubsets.map(p => Chord(p.toList, scale))
  }


//  private def pitches(chord: Chord): List[MidiPitch] = {
//    chord.scaleDegrees.map(d => chord.scale.degreeMap(d))
//  }

  //old implementation - profiling
//  private def getIntervals(pitches: Set[MidiPitch]): Set[Interval] = {
//    (for {
//      (i, ix) <- pitches.zipWithIndex
//      (j, jx) <- pitches.zipWithIndex
//      if (ix < jx)
//    } yield (i - j).abs)
//      .map(interval => intervals.get(interval % 12).get)
//  }

  private def getIntervals(pitches: Set[MidiPitch]): Iterator[Interval] = {
    pitches.subsets(2).map(i => {
      val intervalValue = (i.min - i.max).abs % 12
      intervals.get(intervalValue).get
    })
  }

  private def getInfo(chord: Chord): ChordInfo = {
    val pitches = chord.getPitches().toSet

    //all pitch pairs in the chord permutation
    val intervalsInChord = getIntervals(pitches).toList

    val dissonance = intervalsInChord.map(i => i.dissonanceRank).sum
    val names = intervalsInChord.map(_.name)
    val consecutiveDegreeDiffs = chord.scaleDegrees.sorted.sliding(2).map { case Seq(x, y, _*) => y - x }
    val unevenness = consecutiveDegreeDiffs.toSet.sum.toFloat / (chord.scaleDegrees.max - chord.scaleDegrees.min)
    val dissonanceOfLargestInterval = intervalsInChord.sortBy(_.tone).last.dissonanceRank


    ChordInfo(
      voicing = chord.scaleDegrees,
      dissonance = dissonance,
      intervalNames = names,
      avgDegree = chord.scaleDegrees.sum.toFloat / chord.scaleDegrees.size.toFloat,
      unevenness = unevenness,
      dissonanceOfLargestInterval = dissonanceOfLargestInterval,
      range = chord.scaleDegrees.max - chord.scaleDegrees.min)
  }
}
