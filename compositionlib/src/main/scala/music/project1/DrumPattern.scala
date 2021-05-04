package music.project1

import generators.Meter.{allSubdivisions, beatIndices, onBeats, theOne}
import generators.RhythmGenerators.bjorklund
import midi.{OutDevices, Sequencer}
import models.ControlSignals.midiNoteNames
import models.DrumNames.{HHC, KICK}
import models.NoteSequences.{PolyphonicScalePhrase, PolyphonicScalePhraseBarConstructor, ScalePhrase}
import models.NullObjects.{nullChord, nullPolyphonicScalePhraseBarConstructor}
import models.Primitives._
import models.Scales.modeNumberMap
import models._
import music.project1.Runner.Project1SharedData
import transformers.RhythmTransformers.{shift, steps2Subdivisions, subtractFromFilled, swing}
import transformers.SequenceTransformers.rotate

object DrumPattern {

  def apply(sd: Project1SharedData) = {

    val clave = sd.clave
    val claveVelocities = sd.claveVelocities

    val quarters = allSubdivisions(4, 4)
    val eighths = allSubdivisions(4, 8)
    val beats = onBeats(4, 4)
    val one = theOne(4, 4)
    val backbeat = beatIndices(4, 4, List(1, 3))

    val beatStrengths = List(100, 75, 75, 75)
    val kick1 = Bar(KICK, beats, beatStrengths)
    val kick = List.fill(4)(kick1)

    //  val hhc1 = Bar(HHC, clave, claveVelocities)
    val b8 = subtractFromFilled(steps2Subdivisions(bjorklund(32, 5, hitDuration = s), 8))
    val hhc1 = Bar(HHC, swing(quarters, 3), 100)
    val hhc = List.fill(4)(hhc1)

    //  val hho1 = Bar(HHO, shift(beats, 0,2), rotate(beatStrengths,2))
    //  val hho = List.fill(4)(hho1)

    //  val hhp1 = Bar(HHP, shift(clave,2,0), claveVelocities)
    //  val hhp = List.fill(4)(hhp1)

    //expand over multiple bars - or
    //  val sn1 = Bar(SNARE, backbeat, List(90, 75))
    //  val sn2 = Bar(SNARE, shift(backbeat,0,1), List(90, 75))
    //  val sn = List(sn1, sn2, sn1, sn2)


    //ideas: double-speed rhythm that still respects the clave
    //create a representation of intervals
    //melodic movement in the chord sequence



    //  val bass = chordRoots.flatMap(r => List.fill(4)(r)).grouped(5).zipWithIndex.map(r => {
    //    val p = ScalePhrase(r._1.map(m => m), scale)
    //    val bassClave = clave.copy(hitDurations = List(w,h,q,w,h))
    //    Bar(p, shift(addSubdivisions(shift(bassClave, 0, r._2%2), 12), 8, 0), rotate(claveVelocities, r._2))
    //  }).toList


    //  val drumLine: ParallelBarSequences = List(BarSequence(kick,1), BarSequence(hhc,1), BarSequence(hho,1), BarSequence(hhp,1), BarSequence(sn,1))
    val drumLine = List(BarSequence(kick, 1), BarSequence(hhc, 1))
    //  val bassLine = List(BarSequence(bass, 3))

    val arrangement = Arrangement(drumLine).repeat(2)

    arrangement

  }
}
