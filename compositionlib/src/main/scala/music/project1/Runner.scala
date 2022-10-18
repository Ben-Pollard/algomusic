package music.project1

import enums.MidiNoteNames
import generators.RhythmGenerators.generateBjorklundSequence
import instruments.{Harp, TonalInstrument, Violins1}
import midi.Sequencer
import models.midibuilders.ArrangementConstruction.{BarInfo, SequenceInfo}
import models.Primitives.q
import models.Scales.modeNumberMap
import models.midibuilders.{Arrangement, VoiceJoining}
import models.{Rhythm, Scale}
import util.NullObjects.nullPolyphonicScalePhraseBarConstructor

object Runner extends App {

  val startTimeMillis = System.currentTimeMillis()

  case class Project1SharedData(clave: Rhythm, scale: Scale, chordRoots: List[Int], chordDegrees: List[Int], sequenceIndices: List[List[SequenceInfo]])

  val clave = generateBjorklundSequence(16, 5, hitDuration = q, List(100, 75, 65, 90, 75)).steps2Subdivisions(4)
  clave.info()

//  val circleOfFifths = List(1, 4, 7, 2, 5, 8, 3, 6).map(_ - 1)
  val chordRoots = List(1, 3 - 8, 6 - 8, 7 - 8, 5 - 8).map(_ - 1)
  val chordDegrees = List(1, 3, 5).map(_ - 1)

  val nullSequenceInfo = List(SequenceInfo(0, 0, 0))
  val scanSeed = BarInfo(nullPolyphonicScalePhraseBarConstructor(0), nullPolyphonicScalePhraseBarConstructor(0), nullSequenceInfo)

  val sequenceIndices = chordRoots //5 notes in rhythm. change chords every 4
    .map(r => List.fill(4)(r))
    .zipWithIndex
    .flatMap(i => i._1.zip(List.fill(i._1.length)(i._2)))
    .grouped(5)
    .zipWithIndex
    .map(i => i._1.zip(List.fill(i._1.length)(i._2)).map(j => SequenceInfo(j._1._1, j._1._2, j._2)))
    .toList.par

//  val sd = Project1SharedData(clave, scale, chordRoots, chordDegrees, sequenceIndices)

  // construct a base harmony across the midi range
  // then adapt it for each instrument
  def scale(instrument: TonalInstrument) = {
    Scale(modeNumberMap.get(3).get, MidiNoteNames.D.id, instrument)
  }

  val harp = Harp(6, "C3", "G7") // restricting the note range reduces voicing permutation overhead
  val harpHarmony = Harmony(clave, scale(harp), chordRoots, chordDegrees, sequenceIndices) //todo contruct based on base harmony
  val harmonicRhythmArrangement = Arrangement(harpHarmony, harp)

  val padHarmony = Harmony(clave, scale(Violins1(8)), chordRoots, chordDegrees, sequenceIndices) //todo contruct based on base harmony
  val padArrangement = Arrangement(padHarmony, Violins1(8), Some(VoiceJoining(expand = true, join = true)))

//  val fluteHarmony = Harmony(clave, scale(Flutes(3)), chordRoots, chordDegrees, sequenceIndices) //todo contruct based on base harmony
//  val melodyArrangement = Arrangement(Melody(fluteHarmony), Flutes(1))
//
//  val oboeHarmony = Harmony(clave, scale(Oboes(1)), chordRoots, chordDegrees, sequenceIndices) //todo contruct based on base harmony
//  val counterPointArrangement = Arrangement(CounterPoint(oboeHarmony), Oboes(1))

  val endTimeMillis = System.currentTimeMillis()
  val durationSeconds = (endTimeMillis - startTimeMillis)
  println(s"Built arrangement in ${durationSeconds}ms")

//  Sequencer(harmonicRhythmArrangement).play(60)
  Sequencer(padArrangement ++ harmonicRhythmArrangement).play(60)
//  Sequencer(padArrangement ++ harmonicRhythmArrangement ++ melodyArrangement ++ counterPointArrangement).play(60, repeat = 1)

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
  // Data structure for metadata, e.g. matrix, dataframe
  // Visualise
  // Map existing structure based on loops to the data structure
  // Define parameters - tension/release, emphasis - do some research
  // Map control data to metastructure
  // Combine internal bar dynamics with metastructure and map to expression / articulation control

  // ## Music driver
  // Boringness analyser - see notebook

  // ## Harmony
  // Change the key
  // Change the mode
  // Build chord progressions from metastructure

  // ## Rhythm
  // Simple polyrhythms
  // Percussive expression (adsr)
  // Rhythmic response to metastructure

  // ## Instruments
  // Check out Google NSynth

  // ## DAW templating
  // Check out REAPER
  // Check out bitwig java api

}
