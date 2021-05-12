package models

import models.Primitives.{MidiPitch, Octave, ScaleDegree, Tone, midiRange}
import models.Scales.majorScalePattern

//SCALE
case class Scale(pitches: Seq[MidiPitch], root: MidiPitch, degreeMap: Map[ScaleDegree, MidiPitch], pattern: Seq[Tone]) {

  def getDegreePitch(d: Int) = {
    degreeMap.get(d).get
  }

  def getDegreeIndex(d:Int) = {
    pitches.indexOf(getDegreePitch(d))
  }

}

object Scale {
  def apply(pattern: Seq[Tone], root: MidiPitch):Scale = {
    assert(pattern.sum==12)

    val degreeMap: Map[ScaleDegree, MidiPitch] = Seq.fill(10)(pattern.scanLeft(0)(_+_).dropRight(1).map(_+root))
      .map(_.zipWithIndex).zipWithIndex
      .flatMap(i => i._1.map(j => (j._1 + 12*(i._2-5), j._2 + 1 + 7*(i._2-5))))
      .map(_.swap)
      .filter(x => x._2>0 & x._2<=midiRange.max)
      .toMap

    val degreeOctaveMap: Map[(ScaleDegree, Octave), MidiPitch] = Seq.fill(10)(majorScalePattern.scanLeft(0)(_+_).dropRight(1).map(_+root))
      .map(_.zipWithIndex).zipWithIndex
      .flatMap(i => i._1.map(j => (j._1 + 12*(i._2-5), (j._2 + 1, (i._2-5)))))
      .map(_.swap)
      .filter(x => x._2>0 & x._2<=midiRange.max)
      .toMap


    val pitches = degreeMap.values.toList.sorted

    new Scale(pitches, root, degreeMap, pattern)
  }
}
