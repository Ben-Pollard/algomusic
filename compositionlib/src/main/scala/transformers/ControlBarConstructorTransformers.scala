package transformers

import generators.ControlSignals.generateSineWave
import models.Primitives.MidiCCNum
import models.barconstructors
import models.barconstructors.{AbstractPolyphonicScalePhraseBarConstructor, PolyphonicScalePhraseBarConstructor}

trait ControlBarConstructorTransformers extends AbstractPolyphonicScalePhraseBarConstructor{

  def applySineToEachNote(midiCCNum: MidiCCNum): PolyphonicScalePhraseBarConstructor = {
    val noteStartEndTimes = rhythm.getNoteStartsAndEnds()
    val resolution = 10
    val controlRhythm = rhythm.toControlRhythm(resolution)
    val ccLevels = rhythm.applySignalToEachNote(generateSineWave, noteStartEndTimes, resolution)
    val controlBarConstructor = Some(barconstructors.CCBarConstructor(midiCCNum, ccLevels, controlRhythm))
    PolyphonicScalePhraseBarConstructor(scalePhrases, rhythm, controlBarConstructor)
  }

}
