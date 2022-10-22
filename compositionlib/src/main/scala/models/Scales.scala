package models

import enums.Modes._
import enums.{MidiNoteNames, Modes}
import instruments.DefaultInstrument
import models.Primitives.{Tone, semiTone, tone}
import util.SequenceTransformers

object Scales {

  val majorScalePattern = Seq(tone,tone,semiTone,tone,tone,tone,semiTone)

  val modes = List(Ionian, Dorian, Phrygian, Lydian, Mixolydian, Aeolian, Locrian)

  val modeNameMap: Map[Modes.Value, Seq[Tone]] = modes.map(m => (m, SequenceTransformers.reverseRotate(majorScalePattern, m.id))).toMap

  val modeNumberMap: Map[Int, Seq[Tone]] = modeNameMap.map(m => (m._1.id, m._2))

  def getModeByNumber(modeNumber: Int) = {
    modeNumberMap.get(modeNumber).get
  }


  val CMaj = Scale(majorScalePattern, MidiNoteNames.C.id, DefaultInstrument(0,0, "C0", "C10"))


}
