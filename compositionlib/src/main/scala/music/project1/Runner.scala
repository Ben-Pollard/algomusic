package music.project1

import generators.RhythmGenerators.bjorklund
import midi.{OutDevices, Sequencer}
import models.ControlSignals.midiNoteNames
import models.NullObjects.nullPolyphonicScalePhraseBarConstructor
import models.Primitives.{ScaleDegree, Velocity, q}
import models.{Rhythm, Scale}
import models.Scales.modeNumberMap
import transformers.RhythmTransformers.steps2Subdivisions

object Runner extends App {

  case class SequenceInfo(degree: ScaleDegree, chordNum: Int, barNum: Int)
  case class BarInfo[A, B](oldConstructor: A, newConstructor: A, sequenceInfo: List[B])
  case class Project1SharedData(clave: Rhythm, scale: Scale, chordRoots: List[Int], chordDegrees: List[Int], sequenceIndices: List[List[SequenceInfo]])

  val clave = steps2Subdivisions(bjorklund(16, 5, hitDuration = q, List(100, 75, 65, 90, 75)), 4)
  clave.info()

  var scale = Scale(modeNumberMap.get(3).get, midiNoteNames.get("D").get)
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
    .toList

  val sd = Project1SharedData(clave, scale, chordRoots, chordDegrees, sequenceIndices)

//  Sequencer(DrumPattern(sd) ++ Harmony(sd), bpm = 60, midiDevice = OutDevices.LOOP_MIDI_PORT)
  Sequencer(DrumPattern(sd) ++ CounterPoint(sd), bpm = 60, midiDevice = OutDevices.LOOP_MIDI_PORT)

}
