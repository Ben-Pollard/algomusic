package transformers

import generators.ControlSignals.generateSineWave
import midi.MidiControlNumbers
import models.barconstructors
import models.barconstructors.{AbstractPolyphonicScalePhraseBarConstructor, PolyphonicScalePhraseBarConstructor}

trait ControlBarConstructorTransformers extends AbstractPolyphonicScalePhraseBarConstructor{

  def applySineToEachNote(): PolyphonicScalePhraseBarConstructor = {
    val noteStartEndTimes = rhythm.getNoteStartsAndEnds()
    val resolution = 10
    val controlRhythm = rhythm.toControlRhythm(resolution)
    val ccLevels = rhythm.applySignalToEachNote(generateSineWave, noteStartEndTimes, resolution)
    val controlBarConstructor = Some(barconstructors.CCBarConstructor(MidiControlNumbers.MODULATION_WHEEL_OR_LEVER, ccLevels, controlRhythm))
    PolyphonicScalePhraseBarConstructor(scalePhrases, rhythm, controlBarConstructor)
  }

}
