package models

import instruments.Instrument
import models.ControlSequences.CCBarConstructor
import models.Primitives.{CCRest, MidiCC}


object ControlBar {

  //constructor for bar of control values
  def apply(constructor: CCBarConstructor, instrument: Instrument): Bar = {

    val (midiCCNum, midiCCValues, rhythm) = (constructor.midiCCNum.id, constructor.midiCCValues, constructor.rhythm)

    assert(midiCCValues.length == rhythm.hitIndices.length)
    assert(rhythm.durations.filter(_.isLeft).length == midiCCValues.length)

    val onNotes = midiCCValues zip rhythm.durations.filter(_.isLeft) map { n => MidiCC(
      duration = n._2.left.get,
      number = midiCCNum,
      value = Some(n._1)) }

    val rests = rhythm.durations.filter(_.isRight).map(r => CCRest(r.right.get))

    val notes = rests.zipAll(onNotes, CCRest(0), CCRest(0)).flatMap(pair => List(pair._1, pair._2))

    //correct any rounding errors in duration
    val roundingError = notes.map(_.duration).sum.round - notes.map(_.duration).sum
    val roundedNotes = notes.zipWithIndex.map(n => if (n._2== notes.length/2) n._1.copy(duration = n._1.duration + roundingError) else n._1)


    Bar(roundedNotes :: Nil, instrument)
  }


}
