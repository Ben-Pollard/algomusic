package music.project1

import breeze.linalg.{DenseMatrix, SliceMatrix}
import enums.DrumNames._
import enums.MidiNoteNames
import generators.Meter.{allSubdivisions, theOne}
import generators.RhythmGenerators.generateBjorklundSequence
import instruments._
import midi.Sequencer
import models.Primitives.q
import models.{PolyphonicScalePhrase, Scale}
import models.Scales.{getModeByNumber, modeNumberMap}
import models.barconstructors.PolyphonicScalePhraseBarConstructor
import models.midibuilders.ArrangementConstruction.{BarInfo, SequenceInfo}
import models.midibuilders.{Arrangement, Bar, ControlBar, Track, VoiceJoining}
import util.NullObjects.nullPolyphonicScalePhraseBarConstructor

import scala.collection.immutable
import scala.collection.parallel.ParSeq

object Runner extends App {

  val startTimeMillis = System.currentTimeMillis()

  val nullSequenceInfo = List(SequenceInfo(0, 0, 0))
  val nullmatrix = DenseMatrix(List(0.0)).t
  val nullslicer = nullmatrix(::,0) :== 0.0
  val nullslicematrix = nullmatrix(nullslicer, ::)
  val scanSeed = BarInfo(nullPolyphonicScalePhraseBarConstructor(0), nullslicematrix)

    val clave = generateBjorklundSequence(16, 5, hitDuration = q, List(100, 75, 65, 90, 75)).steps2Subdivisions(4)
  clave.info()

//  val circleOfFifths = List(1, 4, 7, 2, 5, 8, 3, 6).map(_ - 1)
  val chordRoots = List(1, 3 - 8, 6 - 8, 7 - 8, 5 - 8).map(_ - 1)
  val chordDegrees = List(1, 3, 5).map(_ - 1)


  val sequenceIndices = chordRoots //5 notes in rhythm. change chords every 4
    .map(r => List.fill(4)(r))
    .zipWithIndex
    .flatMap(i => i._1.zip(List.fill(i._1.length)(i._2)))
    .grouped(5)
    .zipWithIndex
    .map(i => i._1.zip(List.fill(i._1.length)(i._2)).map(j => SequenceInfo(j._1._1, j._1._2, j._2)))
    .toList.par

  // construct a base harmony across the midi range
  // then adapt it for each instrument
  def scale(instrument: TonalInstrument) = {
    Scale(getModeByNumber(3), MidiNoteNames.D.id, instrument)
  }

//  val baseHarmony = BaseHarmony(clave, scale(DefaultInstrument(0, 8, "C3", "B3")), chordRoots, chordDegrees, sequenceIndices)
  val baseHarmony = BaseHarmonyFromControlMatrix(scale(DefaultInstrument(0, 8, "C3", "B3")))


  val harp = Harp(6, "C3", "G7") // restricting the note range reduces voicing permutation overhead
  val harpHarmony = RevoiceWithLeading(scale(harp), baseHarmony)
  val harmonicRhythmArrangement = Arrangement(harpHarmony, harp)

  val violin = Violin(8)
  val padHarmony = RevoiceWithLeading(scale(violin), baseHarmony)
  val padArrangement = Arrangement(padHarmony, violin, Some(VoiceJoining(expand = true, join = true)))

  val kit = LabsDrums
  val drums = baseHarmony.map(bar => {
    val chordIndices = bar.controlData(::,1).toScalaVector.map(_.toInt).toList
    val durations = bar.controlData(::,2).toScalaVector
    val velocities = bar.controlData(::,3).toScalaVector.map(_.toInt)

    // our metastructure is organised around the harmonic rhythm
    // rhythmic elements will have differing time signatures
    // to be useful we need to interpolate signals onto new rhythms
    // take velocity as an example: interpolating 5 velocities onto a 16-step pattern
    // we need to know where the hit indices are within the 16 steps
    // that is defined within the harmonic rhythm generator
    velocities.length

    val sixteenths = allSubdivisions(4,8)
    Bar(KICK, theOne(4,4), kit) +
    Bar(SNARE, theOne(4,4).rotate(1), kit) +
    Bar(HHP, bar.constructor.rhythm, kit) + // this is the 5/16
      Bar(HHC, sixteenths.subtract(bar.constructor.rhythm), kit) +
      ControlBar(bar.constructor.controlBarConstructor.get, kit)
  })
  val drumArrangement = Arrangement(List((Track(drums, kit))))



//  val fluteHarmony = RevoiceWithLeading(scale(Flute(3)), baseHarmony)
//  val melodyArrangement = Arrangement(Melody(fluteHarmony), Flute(1))
//
//  val oboeHarmony = RevoiceWithLeading(scale(Oboe(3)), baseHarmony)
//  val counterPointArrangement = Arrangement(CounterPoint(oboeHarmony), Oboe(1))

  val endTimeMillis = System.currentTimeMillis()
  val durationSeconds = (endTimeMillis - startTimeMillis)
  println(s"Built Arrangement in ${durationSeconds}ms")


  Sequencer(padArrangement ++ harmonicRhythmArrangement ++ drumArrangement).play(60)
//  Sequencer(padArrangement ++ harmonicRhythmArrangement ++ melodyArrangement ++ counterPointArrangement).play(60)

  // todo
  // CC
  // Choose to apply signals to notes or bars
  // Separate expression types - on beat and smoothed
  // Create more signals - vibrato, lognorm, reverse lognorm
  // Map the signals to notes based on note length
  // Apply signals based on track structure data

  // ## Expression
  // map expression data to bar dynamics
  // extend to two cc parameters - keep it within a single control bar
  // send articulation control information
  // vibrato (change in pitch, also maybe in amplitude/volume)
  // Use vibrato + reverse lognorm to introduce vibrato as a note is sustained
  // adsr
  // velocity response e.g. changes to drum adsr
  // timbre - research and map to plugins

  // ## Track structure
  // Modify cc data based on metastructure


  // ## Music driver
  // Boringness analyser - see notebook

  // ## Harmony
  // Change the key
  // Change the mode
  // Build chord progressions from metastructure

  // ## Rhythm
  // Rhythmic response to metastructure
  // Percussive expression (adsr)


  // ## Instruments
  // Check out Google NSynth

  // ## DAW templating
  // Check out REAPER
  // Check out bitwig java api

}
