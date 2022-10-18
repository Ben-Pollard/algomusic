package models

import instruments.TonalInstrument
import models.Primitives.{MidiPitch, PitchName, ScaleDegree, Tone}

//SCALE
case class Scale(pitches: Seq[MidiPitch], root: MidiPitch, degreeMap: Map[ScaleDegree, MidiPitch], pattern: Seq[Tone], instrument: TonalInstrument) {

  def getDegreePitch(d: Int) = {
    degreeMap.get(d).get
  }

  def getDegreeIndex(d:Int) = {
    pitches.indexOf(getDegreePitch(d))
  }

  def getNoteNameMap(): List[(ScaleDegree, PitchName)] = {
    val pitchNameMap: Map[MidiPitch, PitchName] = instrument.midiMap().map(_.swap)

    degreeMap
      .toList.sorted
      .map(pair => {
      val (degree, pitch) = pair
      (degree, pitchNameMap.get(pitch).get)
    })
  }

}

object Scale {
  def apply(pattern: Seq[Tone], root: MidiPitch, instrument: TonalInstrument): Scale = {
    assert(pattern.sum==12)

    val degreeMap: Map[ScaleDegree, MidiPitch] = Seq.fill(10)(pattern.scanLeft(0)(_+_).dropRight(1).map(_+root))
      .map(_.zipWithIndex).zipWithIndex
      .flatMap(i => i._1.map(j => (j._1 + 12*(i._2-5), j._2 + 1 + 7*(i._2-5))))
      .map(x => (x._2, x._1))
      .filter(x => x._2>=instrument.lowestPitch & x._2<=instrument.highestPitch)
      .toMap

//    val degreeOctaveMap: Map[(ScaleDegree, Octave), MidiPitch] = Seq.fill(10)(majorScalePattern.scanLeft(0)(_+_).dropRight(1).map(_+root))
//      .map(_.zipWithIndex).zipWithIndex
//      .flatMap(i => i._1.map(j => (j._1 + 12*(i._2-5), (j._2 + 1, (i._2-5)))))
//      .map(x => (x._2, x._1.asInstanceOf[MidiPitch]))
//      .filter(x => x._2>0 & x._2<=midiRange.max)
//      .toMap

    val pitches = degreeMap.values.toList.sorted

    new Scale(pitches, root, degreeMap, pattern, instrument)
  }
}
