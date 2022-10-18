package transformers

import generators.ControlSignals
import midi.MidiControlNumbers
import models.Primitives.{Duration, MidiCCValue, RestDuration, Velocity}
import models.{Rhythm, barconstructors}
import models.barconstructors.{AbstractPolyphonicScalePhraseBarConstructor, PolyphonicScalePhraseBarConstructor}
import util.Util

trait ControlBarConstructorTransformers extends AbstractPolyphonicScalePhraseBarConstructor{

  def controlTest(): PolyphonicScalePhraseBarConstructor = {
    val ccLevel: Seq[MidiCCValue] = rhythm.hitDurations.map(d => Util.scaleToByte(1, d.toInt))

    assert(rhythm.durations.head.isRight)

    // get start/end times of notes. this can go on rhythm
    val timestamps = rhythm.durations.map(_ match {
      case Left(duration: Duration) => duration
      case Right(duration: RestDuration) => duration
    }).scanLeft(0.0)((a, b) => a + b).tail

    val startEndTimes = timestamps.sliding(2, 2).toList.dropRight(1)

    // Construct control rhythm of 10x resolution. also on rhythm
    val resolution = 10
    val steps: Int = rhythm.beats * rhythm.subdivisions * resolution
    val hitIndices: Seq[Int] = startEndTimes.flatMap(t => (((t(0) * resolution * rhythm.subdivisions).toInt to (t(1) * resolution * rhythm.subdivisions).toInt)))
    val hitDurations: Seq[Duration] = List.fill(hitIndices.size)(1)
    val velocities: Seq[Velocity] = List()
    val controlRhythm = Rhythm(steps, hitIndices, hitDurations, velocities).setBeatsPerBar(rhythm.beats)

    // Construct ccLevels from rhythm data
    // Each note lasts a different length of time, so we need copies of a control signal that are scaled to the correct number of steps
    val ccLevels: Seq[MidiCCValue] = startEndTimes.flatMap(t => {
      val startTime = (t(0) * resolution * rhythm.subdivisions).toInt
      val endTime = (t(1) * resolution * rhythm.subdivisions).toInt
      ControlSignals.generateSineWave(1 + endTime - startTime).scaleToRange(100, 120)
    })

    val controlBarConstructor = Some(barconstructors.CCBarConstructor(MidiControlNumbers.MODULATION_WHEEL_OR_LEVER, ccLevels, controlRhythm))
    PolyphonicScalePhraseBarConstructor(scalePhrases, rhythm, controlBarConstructor)

  }

}
