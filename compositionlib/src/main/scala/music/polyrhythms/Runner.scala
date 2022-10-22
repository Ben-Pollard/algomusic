package music.polyrhythms

import enums.DrumNames._
import enums.MidiNoteNames
import generators.Meter.theOne
import generators.RhythmGenerators
import instruments.CC_FPC.{FILTER_CUTOFF, HHC_ATTACK, HHC_DECAY, HHC_RELEASE, HHC_SUSTAIN, HHO_ATTACK, HHO_DECAY, HHO_RELEASE, HHO_SUSTAIN, KICK_ATTACK, KICK_DECAY, KICK_RELEASE, KICK_SUSTAIN}
import instruments.{DefaultInstrument, FPC}
import midi.Sequencer
import models.Primitives._
import models.Scales.modeNumberMap
import models._
import models.barconstructors.{CCBarConstructor, PolyphonicScalePhraseBarConstructor}
import models.midibuilders.{Arrangement, Bar, ControlBar, Track}
import util.NullObjects.emptyRhythm
import util.Util.{lowestCommonMultiple, possibleTimeSigs, scaleToByte}

object Runner extends App {

  val beatsPerBar = List(3,4)
  val totalSubdivs = 2 * lowestCommonMultiple(beatsPerBar)
  possibleTimeSigs(totalSubdivs)
  val beats = 4
  val subDivsPerBeat = totalSubdivs / beats

  val beatStrengths_4_6 = Vector(100, 80, 90, 75)
  val subDivStrength_4_6 = Vector(100, 75, 75, 90, 75, 75)

  val beatDurations_4_6 = Vector(w, q, h, q)
  val subDivDurations_4_6 = Vector(w, q, q, h, q, q)

  val beatStrengthMap_4_6 = (0 until (beatStrengths_4_6.size * subDivStrength_4_6.size)).map(i => {
    val beatNum = i / subDivStrength_4_6.size
    val subDivNum = i % subDivStrength_4_6.size
    val strength = (beatStrengths_4_6(beatNum) + subDivStrength_4_6(subDivNum)) / 2
    val duration = (beatDurations_4_6(beatNum) + subDivDurations_4_6(subDivNum)) / 2
    val index = (beatNum, subDivNum)
    (index, (strength, duration))
  }).toMap
  println(beatStrengthMap_4_6)

  //todo durations
  //can be 1/24 to 1

  val beatStrengths_3_8 = Vector(100, 75, 75)
  val subDivStrength_3_8 = Vector(100, 75, 80, 75, 90, 75, 80, 75)

  val beatStrengthMap_3_8 = (0 until (beatStrengths_3_8.size * subDivStrength_3_8.size)).map(i => {
    val beatNum = i / subDivStrength_3_8.size
    val subDivNum = i % subDivStrength_3_8.size
    val strength = (beatStrengths_3_8(beatNum) + subDivStrength_3_8(subDivNum)) / 2
    val index = (beatNum, subDivNum)
    (index, strength)
  }).toMap
  println(beatStrengthMap_3_8)

  //we can build the rhythms in a 4/3 way and a 3/4 way
  //then map the 3/8s onto 4/6 so we have an even number of beats per bar
  //the total number of subdivs will affect the way the euclidean rhythms are spaced and how rotations work
  val rhythms_4_3 = emptyRhythm(totalSubdivs).setBeatsPerBar(beats) +: (1 to 6)
    .map(i => {
      //beat strength: stronger if it falls on a beat and if it falls on a strong beat
      val rhythm = RhythmGenerators.generateBjorklundSequence(totalSubdivs,i, h).setBeatsPerBar(beats)
      val velocities = rhythm.hitIndices.map(i => beatStrengthMap_4_6((i._1, i._2))._1)
      val durations = rhythm.hitIndices.map(i => beatStrengthMap_4_6((i._1, i._2))._2)
      rhythm.setVelocities(velocities).setDurations(durations)
    })

  val numBars = lowestCommonMultiple(List(6,7))
  val scaleBarNumToByte = (n: Int) => scaleToByte(numBars, n)

  val kickControl = ControlBar(CCBarConstructor(midiCCNum = KICK_ATTACK.id, List(0), rhythms_4_3(1)), FPC) +
    midibuilders.ControlBar(barconstructors.CCBarConstructor(midiCCNum = KICK_DECAY.id, List(64), rhythms_4_3(1)), FPC) +
      midibuilders.ControlBar(barconstructors.CCBarConstructor(midiCCNum = KICK_SUSTAIN.id, List(64), rhythms_4_3(1)), FPC) +
        midibuilders.ControlBar(barconstructors.CCBarConstructor(midiCCNum = KICK_RELEASE.id, List(64), rhythms_4_3(1)), FPC)

  val hhoControl = midibuilders.ControlBar(barconstructors.CCBarConstructor(midiCCNum = HHO_ATTACK.id, List(0), rhythms_4_3(1)), FPC) +
    midibuilders.ControlBar(barconstructors.CCBarConstructor(midiCCNum = HHO_DECAY.id, List(64), rhythms_4_3(1)), FPC) +
      midibuilders.ControlBar(barconstructors.CCBarConstructor(midiCCNum = HHO_SUSTAIN.id, List(64), rhythms_4_3(1)), FPC) +
        midibuilders.ControlBar(barconstructors.CCBarConstructor(midiCCNum = HHO_RELEASE.id, List(64), rhythms_4_3(1)), FPC)

  val hhcControl = midibuilders.ControlBar(barconstructors.CCBarConstructor(midiCCNum = HHC_ATTACK.id, List(0), rhythms_4_3(1)), FPC) +
    midibuilders.ControlBar(barconstructors.CCBarConstructor(midiCCNum = HHC_DECAY.id, List(64), rhythms_4_3(1)), FPC) +
      midibuilders.ControlBar(barconstructors.CCBarConstructor(midiCCNum = HHC_SUSTAIN.id, List(64), rhythms_4_3(1)), FPC) +
        midibuilders.ControlBar(barconstructors.CCBarConstructor(midiCCNum = HHC_RELEASE.id, List(64), rhythms_4_3(1)), FPC)

  val pianoControl = midibuilders.ControlBar(barconstructors.CCBarConstructor(midiCCNum = FILTER_CUTOFF.id, List(127), rhythms_4_3(1)), FPC)


  var scale = Scale(modeNumberMap.get(3).get, MidiNoteNames.D.id, DefaultInstrument(0,0, "C3", "C5"))
  val chordRoots = List(1, 3 - 8, 6 - 8, 7 - 8, 5 - 8).map(_ - 1)
  val chordDegrees = List(1, 3, 5).map(_ - 1)


  //our goal here is to:
  //1. express the (poly) meter through the drums
  //2. find some harmonic rhythms
  //3. find some complimentary melodic rhythms
  val lines = (1 until numBars).map(barNum => {

    val controlValue = scaleBarNumToByte(barNum-1)

    val poly_4_3 = rhythms_4_3(4) + rhythms_4_3(3)
//    val kick = Bar(KICK, theOne(beats,subDivsPerBeat)) + kickControl
    val kick = midibuilders.Bar(KICK, rhythms_4_3(4), FPC) + kickControl
    val hho = Bar(HHO, rhythms_4_3(1).rotate(3,3), FPC) + hhoControl
    val hhc = midibuilders.Bar(HHC, poly_4_3.dynamics(64,127), FPC) + midibuilders.ControlBar(barconstructors.CCBarConstructor(midiCCNum = HHC_DECAY.id, poly_4_3.dynamics(10,64).velocities.toList, poly_4_3), FPC)
    val hhp = midibuilders.Bar(HHP, poly_4_3, FPC)
    val sn = midibuilders.Bar(SNARE, theOne(beats,subDivsPerBeat).rotate(2), FPC)

    val numBeatsHarm = 2
    val rhythm = rhythms_4_3(numBeatsHarm).dynamics(64,64).rotate(1,0).setDurations(List(2,2))
    val rootNotes = chordRoots.slice(barNum % 2, barNum % 2 + numBeatsHarm)
    val pp = chordDegrees.map(d => ScalePhrase(rootNotes.map(i => i + d), scale))
    val harm = midibuilders.Bar(PolyphonicScalePhraseBarConstructor(PolyphonicScalePhrase(pp), rhythm), FPC) + pianoControl

    (harm,  kick + hhp + hho + hhc + sn)
  })//.take(5)




  //build and release tension
  //control note length
  //use control signals to drive a filter
  val pianoInstrument = DefaultInstrument(2, 10, "C2", "C5")

  val piano = (1 to numBars toList).map(barNum => {
    val rhythm = (rhythms_4_3(barNum % rhythms_4_3.size) + rhythms_4_3((barNum-1) % rhythms_4_3.size)).dynamics(60, 100)
    val rootNotes = List.fill(rhythm.hitIndices.length)(chordRoots(barNum % chordRoots.length))
    val pp = chordDegrees.map(d => ScalePhrase(rootNotes.map(i => i + d), scale))
    val controlValue = ((barNum%2) + 1)*63
    println(s"Control value: $controlValue")
    val controlMessages = List.fill(rhythm.hitIndices.length)(controlValue)
    val controlBar = midibuilders.ControlBar(barconstructors.CCBarConstructor(midiCCNum = FILTER_CUTOFF.id, controlMessages, rhythm), pianoInstrument)
    val barConstructor = barconstructors.PolyphonicScalePhraseBarConstructor(PolyphonicScalePhrase(pp), rhythm)
    midibuilders.Bar(barConstructor, pianoInstrument) + controlBar
  })


  val drumLine = List(Track(lines.map(_._2), FPC))
  val pianoLine = List(Track(lines.map(_._1), pianoInstrument))
  val pianoLine2 = List(Track(piano, pianoInstrument))
  val arrangement = Arrangement(drumLine ++ pianoLine ++ pianoLine2).repeat(1)

  Sequencer(arrangement).play(60)

}
