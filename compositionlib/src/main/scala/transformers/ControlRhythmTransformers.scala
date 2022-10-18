package transformers

import generators.ControlSignals
import models.{AbstractRhythm, ControlSignal, Rhythm}
import models.Primitives.{Duration, MidiCCValue, RestDuration, Velocity}

trait ControlRhythmTransformers extends AbstractRhythm {

  def getNoteStartsAndEnds(): List[Seq[Duration]] = {
    assert(durations.head.isRight)

    val timestamps = durations.map(_ match {
      case Left(duration: Duration) => duration
      case Right(duration: RestDuration) => duration
    }).scanLeft(0.0)((a, b) => a + b).tail

    timestamps.sliding(2, 2).toList.dropRight(1)
  }

  def toControlRhythm(resolution: Int) = {
    // Construct control rhythm of 10x resolution.
    // The rhythm maps to the note durations within the bar
    val startEndTimes = getNoteStartsAndEnds()
    val steps: Int = beats * subdivisions * resolution
    val hitIndices: Seq[Int] = startEndTimes.flatMap(t => (((t(0) * resolution * subdivisions).toInt to (t(1) * resolution * subdivisions).toInt)))
    val hitDurations: Seq[Duration] = List.fill(hitIndices.size)(1)
    val velocities: Seq[Velocity] = List()
    Rhythm(steps, hitIndices, hitDurations, velocities).setBeatsPerBar(beats)
  }

  def applySignalToEachNote(signalGenerator: (Int) => ControlSignal, startEndTimes: List[Seq[Duration]], resolution: Int): Seq[MidiCCValue] = {
    startEndTimes.flatMap(t => {
      val startTime = (t(0) * resolution * subdivisions).toInt
      val endTime = (t(1) * resolution * subdivisions).toInt
      signalGenerator(1 + endTime - startTime)
        .scaleToRange(100, 120)
    })
  }



}
