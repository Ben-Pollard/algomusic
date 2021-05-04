package music

import generators.Meter.{allSubdivisions, beatIndices, onBeats, theOne}
import generators.RhythmGenerators.bjorklund
import midi.{OutDevices, Sequencer}
import models.Bar.ParallelBarSequences
import models.ControlSignals.midiNoteNames
import models.DrumNames._
import models.Interval.{Interval, intervals}
import models.NullObjects.{nullChord, nullPolyphonicScalePhraseBarConstructor}
import models.Primitives._
import models.NoteSequences.{PolyphonicScalePhrase, PolyphonicScalePhraseBarConstructor, ScalePhrase}
import models.Scales.modeNumberMap
import models._
import transformers.ArrangementTransformers.repeatArrangement
import transformers.RhythmTransformers._
import transformers.SequenceTransformers.rotate

object DrumPattern extends App {

  def rhythmInfo(rhythm: Rhythm) = {
    println(s"Steps: ${rhythm.beats}; Subdivisions: ${rhythm.subdivisions}, Hits: ${rhythm.hitIndices}")
  }

  //8 steps = 8/1, 4/2, 2/4
  //Let's say it's in 2/4
  val clave = steps2Subdivisions(bjorklund(16,5, hitDuration=q), 4)
  rhythmInfo(clave)

  val claveVelocities = List(100, 75, 65, 90, 75)

  val quarters = allSubdivisions(4, 4)
  val eighths = allSubdivisions(4, 8)
  val beats = onBeats(4, 4)
  val one = theOne(4,4)
  val backbeat = beatIndices(4,4, List(1,3))

  val beatStrengths = List(100, 75, 75, 75)
  val kick1 = Bar(KICK, beats, beatStrengths)
  val kick = List.fill(4)(kick1)

//  val hhc1 = Bar(HHC, clave, claveVelocities)
  val b8 = subtractFromFilled(steps2Subdivisions(bjorklund(32,5, hitDuration=s), 8))
  val hhc1 = Bar(HHC, swing(quarters,3), 100)
  val hhc = List.fill(4)(hhc1)

//  val hho1 = Bar(HHO, shift(beats, 0,2), rotate(beatStrengths,2))
//  val hho = List.fill(4)(hho1)

//  val hhp1 = Bar(HHP, shift(clave,2,0), claveVelocities)
//  val hhp = List.fill(4)(hhp1)

  //expand over multiple bars - or
//  val sn1 = Bar(SNARE, backbeat, List(90, 75))
//  val sn2 = Bar(SNARE, shift(backbeat,0,1), List(90, 75))
//  val sn = List(sn1, sn2, sn1, sn2)

  var scale = Scale(modeNumberMap.get(3).get, midiNoteNames.get("D").get)
  val circleOfFifths = List(1,4,7,2,5,8,3,6).map(_-1)
  val chordRoots = List(1,3-8,6-8,7-8,5-8).map(_-1)
  val chordDegrees = List(1,3,5).map(_-1)


  //ideas: double-speed rhythm that still respects the clave
  //create a representation of intervals
  //melodic movement in the chord sequence



//  val harmonicPhrases: List[PolyphonicScalePhraseBarConstructor] = chordRoots.flatMap(r => List.fill(4)(r)).grouped(5).zipWithIndex.map(r => { //5 notes in rhythm. change chords every 4
//    val pp = chordDegrees.map(d => ScalePhrase(r._1.map(m => m+d), scale))
//    val rhythm = {
//      val rotatedDurations = clave.copy(hitDurations = rotate(List(w,h,q,w,h).map(_*2), r._2+4))
//      val alternateBarSwing = shift(rotatedDurations, 0, r._2%2)
//      alternateBarSwing
//    }
//    val velocities = rotate(claveVelocities, r._2)//hit a punctuation every 6
//    val barConstructor = PolyphonicScalePhraseBarConstructor(PolyphonicScalePhrase(pp), rhythm, velocities)
//    barConstructor
//  }).toList

  case class SequenceInfo(degree: ScaleDegree, chordNum: Int, barNum: Int)
  val nullSequenceInfo = List(SequenceInfo(0,0,0))
  case class BarInfo[A, B](oldConstructor: A, newConstructor: A, sequenceInfo: List[B])


  val sequenceIndexes = chordRoots //5 notes in rhythm. change chords every 4
    .map(r => List.fill(4)(r))
    .zipWithIndex
    .flatMap(i => i._1.zip(List.fill(i._1.length)(i._2)))
    .grouped(5)
    .zipWithIndex
    .map(i => i._1.zip(List.fill(i._1.length)(i._2)).map(j => SequenceInfo(j._1._1, j._1._2, j._2)))
    .toList

  val harmonicPhrasesWithSequenceIndexing = sequenceIndexes.map(s => {
    val barNum = s.head.barNum
    val pp = chordDegrees.map(d => ScalePhrase(s.map(i => i.degree + d ), scale))
    val rhythm = {
      val rotatedDurations = clave.copy(hitDurations = rotate(List(w,h,q,w,h).map(_*2), barNum + 4))
      val alternateBarSwing = shift(rotatedDurations, 0, barNum % 2)
      alternateBarSwing
    }
    val velocities = rotate(claveVelocities, barNum) //hit a punctuation every 6
    val barConstructor = PolyphonicScalePhraseBarConstructor(PolyphonicScalePhrase(pp), rhythm, velocities)
    BarInfo(barConstructor, barConstructor, s)
  })

  //VOICE LEADING
  //Voice leading needs to know where the chord changes are and have some lookahead/lookbehind
  //Once we start changing things, chord change locations might effectively change - let's preserve the original locations

  val scanSeed = BarInfo(nullPolyphonicScalePhraseBarConstructor, nullPolyphonicScalePhraseBarConstructor, nullSequenceInfo)

  val revoiced = harmonicPhrasesWithSequenceIndexing.map(p => {

    val scale = p.oldConstructor.scalePhrases.phrases.head.scale
    val chords = p.oldConstructor.scalePhrases.phrases.map(_.degreeSequence).transpose

    val reVoicedChords = chords.map(chord => {
      Chord(chord, scale).voicing(false, true, 4)
    })

    val voiceLeadingChords = reVoicedChords.scanRight(nullChord)((a,b) => {
      if (b.scaleDegrees.isEmpty) a else a.leading(b)
    }).init

    val revoicedPhrases = voiceLeadingChords.map(_.scaleDegrees).transpose.map(c => ScalePhrase(c, scale))

    val newConstructor = p.oldConstructor.copy(scalePhrases = PolyphonicScalePhrase(revoicedPhrases))
    p.copy(oldConstructor = newConstructor, newConstructor = newConstructor)
  })
//    .scanLeft(scanSeed)((a,b) => { //between-bar voicings
//      b.
//    })

  val piano = revoiced.map{c => Bar(c.oldConstructor)}



//  val bass = chordRoots.flatMap(r => List.fill(4)(r)).grouped(5).zipWithIndex.map(r => {
//    val p = ScalePhrase(r._1.map(m => m), scale)
//    val bassClave = clave.copy(hitDurations = List(w,h,q,w,h))
//    Bar(p, shift(addSubdivisions(shift(bassClave, 0, r._2%2), 12), 8, 0), rotate(claveVelocities, r._2))
//  }).toList
//
//
//  val melody = chordRoots.flatMap(r => List.fill(4)(r)).grouped(5).zipWithIndex.map(r => {
//    val p = ScalePhrase(r._1.map(m => m+12), scale)
//    Bar(p, shift(clave, 0, r._2%2), rotate(claveVelocities, r._2))
//  }).toList

  //melody ideas
  //establishing the root; leaps; create an expectation; judicious use of repetition; contrapunctal movement wrt harmony
  //how to express: bar start notes -> movement direction -> concordancy wrt harmony
  //start with a rhythm
  //step 1: contrary motion relative to the bar starts or to the changes?



  //This should become the definition of interval: add tone; work out tones from the scale
  type OverloadedClassIndicator = Byte
  case class Motion(direction: Direction, degreeA: ScaleDegree, degreeB: ScaleDegree, overloaded: OverloadedClassIndicator)
  object Motion {
    def apply(direction: Direction, startDegree: ScaleDegree, degreeStep: Int): Motion = {
      val degreeB = direction match {
        case UP => startDegree + degreeStep
        case DOWN => startDegree - degreeStep
        case NO_DIRECTION => startDegree
      }
      new Motion(direction=direction, degreeA=startDegree, degreeB=degreeB, 1.asInstanceOf[OverloadedClassIndicator])
    }
  }




  //Goals for this melody:
  //construct the phrase from the current outline note and the next outline note
  //harmonic logic for outline notes - be aware of current note & next note in harmony
  //pull through the sequencing data



  val harmonisedMelody = revoiced.map(c => {
    val transposed = c.oldConstructor.copy(scalePhrases = c.oldConstructor.scalePhrases.transpose(7))
    BarInfo(transposed, transposed, c.sequenceInfo)
  }).scanLeft(scanSeed)((a,b) => {
    //In this scan we outline the melody
    val AEmpty = a.oldConstructor.rhythm.beats == 0
    val firstDegreeBOld = b.oldConstructor.scalePhrases.phrases.head.degreeSequence.head
    val firstDegreeAOld = if (AEmpty) firstDegreeBOld else a.oldConstructor.scalePhrases.phrases.head.degreeSequence.head
    val firstDegreeBNew = b.newConstructor.scalePhrases.phrases.head.degreeSequence.head
    val firstDegreeANew = if (AEmpty) firstDegreeBNew else a.newConstructor.scalePhrases.phrases.head.degreeSequence.head

    val direction = Direction(firstDegreeAOld, firstDegreeBOld)
    val motion = Motion(direction.opposite, firstDegreeANew, 1)

    val roots = b.oldConstructor.scalePhrases.phrases.take(1)

    val newPhrases = roots.map(p => {
      val degreeSequence = List(motion.degreeB)
      p.copy(degreeSequence = degreeSequence)
    })

    val newRhythm = b.oldConstructor.rhythm.take(1)
    val newVelocities = b.oldConstructor.velocities.take(1)


    val newConstructor = PolyphonicScalePhraseBarConstructor(PolyphonicScalePhrase(newPhrases), newRhythm, newVelocities)
    BarInfo(b.oldConstructor, newConstructor, b.sequenceInfo)
  }).tail
    .scanRight(scanSeed)((a,b) => {
      //in this scan we colour it in
      val BEmpty = b.oldConstructor.rhythm.beats == 0
      //we need to get to a barconstructor so we can call the infiller
      val targetNote = if (BEmpty) 1 else b.newConstructor.roots.scalePhrase.degreeSequence.head //start of next bar - if last bar, 1? idk, we should get this from a reduce or something
      val newConstructor = a.newConstructor.roots.scalePhraseRunFiller(a.oldConstructor.rhythm, a.oldConstructor.velocities, targetNote).toPoly
      BarInfo(a.oldConstructor, newConstructor, a.sequenceInfo)
    })
    .init
    .map(bi => Bar(bi.newConstructor))





//  val drumLine: ParallelBarSequences = List(BarSequence(kick,1), BarSequence(hhc,1), BarSequence(hho,1), BarSequence(hhp,1), BarSequence(sn,1))
  val drumLine: ParallelBarSequences = List(BarSequence(kick,1), BarSequence(hhc,1))
  val pianoLine: ParallelBarSequences = List(BarSequence(piano, 2))
//  val bassLine: ParallelBarSequences = List(BarSequence(bass, 3))
  val melodyLine: ParallelBarSequences = List(BarSequence(harmonisedMelody, 5))
  val arrangement: ParallelBarSequences = repeatArrangement(pianoLine ++ melodyLine, 2)

  Sequencer(arrangement, bpm=60, midiDevice=OutDevices.LOOP_MIDI_PORT)

}
