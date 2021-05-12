package music.project1

import generators.Meter.{allSubdivisions, beatIndices, onBeats, theOne}
import generators.RhythmGenerators.bjorklund
import models.DrumNames._
import models._
import music.project1.Runner.Project1SharedData

object DrumPattern {

  def apply(sd: Project1SharedData) = {

    val clave = sd.clave

    val quarters = allSubdivisions(4, 4)
    val eighths = allSubdivisions(4, 8)
    val beats = onBeats(4, 4)
    val one = theOne(4, 4)
    val backbeat = beatIndices(4, 4, List(1, 3))

    //to express a polyrhythm naturally we need bars to be running at different speeds
    //But the whole piece needs a fixed bpm
    //So let's fix a number of beats per bar, then apply transformations to beats expressed in different numbers of beats
    //Or is this just another way of saying take two beats and spread them out over six?
    val beatsPerBar = 6
    val _2 = bjorklund(6,2, List(100, 75))
    val _3 = bjorklund(6,3, List(90, 75, 75))
//    val _2_2 = onBeats(2,1)
//    val _3_2 = onBeats(3,1)

    val beatStrengths = List(100, 75, 75, 75)


    val kick1 = Bar(KICK, _2)
    val kick = List.fill(2)(kick1)

//    //  val hhc1 = Bar(HHC, clave, claveVelocities)
//    val b8 = subtractFromFilled(steps2Subdivisions(bjorklund(32, 5, hitDuration = s), 8))
//    val hhc1 = Bar(HHC, swing(quarters, 3), 100)
//    val hhc = List.fill(1)(hhc1)
//
//    val hho1 = Bar(HHO, shift(beats, 0,2), rotate(beatStrengths,2))
//    val hho = List.fill(1)(hho1)
//
//    val hhp1 = Bar(HHP, shift(clave,2,0), claveVelocities)
//    val hhp = List.fill(1)(hhp1)
//

    val sn1 = Bar(SNARE, _3)
    val sn = List.fill(2)(sn1)


    val drumLine = List(BarSequence(kick,1), BarSequence(sn,1))
    val arrangement = Arrangement(drumLine).repeat(1)
    arrangement
  }
}
