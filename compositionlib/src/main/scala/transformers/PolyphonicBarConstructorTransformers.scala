package transformers

import enums.VoicingQualities.VoicingQuality
import instruments.TonalInstrument
import models.barconstructors.{AbstractPolyphonicScalePhraseBarConstructor, PolyphonicScalePhraseBarConstructor, ScalePhraseBarConstructor}
import models.{Chord, PolyphonicScalePhrase, Scale, ScalePhrase, barconstructors}
import util.NullObjects.nullChord


trait PolyphonicBarConstructorTransformers extends AbstractPolyphonicScalePhraseBarConstructor {

  def roots(): ScalePhraseBarConstructor = {
    barconstructors.ScalePhraseBarConstructor(scalePhrases.phrases.head, rhythm)
  }

  def revoice(staticRoot: Boolean, allowInversions: Boolean, voicingQuality: VoicingQuality, qualityRank: Int = 0, scale: Scale): PolyphonicScalePhraseBarConstructor = {

    val chords = scalePhrases.phrases.map(_.degreeSequence).transpose

    val reVoicedChords = chords.map(chord => {
      Chord(chord, scale).voicing(allowInversions, voicingQuality, qualityRank, scale.instrument.voices)
    })

    val revoicedPhrases = reVoicedChords.map(_.scaleDegrees).transpose.map(c => ScalePhrase(c, scale))

    PolyphonicScalePhraseBarConstructor(PolyphonicScalePhrase(revoicedPhrases), rhythm, controlBarConstructor)
  }


  def leading(): PolyphonicScalePhraseBarConstructor = {
    val scale = scalePhrases.phrases.head.scale
    val chords = scalePhrases.phrases.map(_.degreeSequence).transpose.map(c => Chord(c, scale))

    val voiceLeadingChords = chords.scanRight(nullChord)((a, b) => {
      if (b.scaleDegrees.isEmpty) a else a.leading(b)
    }).init

    val leadingPhrases = voiceLeadingChords.map(_.scaleDegrees).transpose.map(c => ScalePhrase(c, scale))

    PolyphonicScalePhraseBarConstructor(PolyphonicScalePhrase(leadingPhrases), rhythm, controlBarConstructor)
  }



}
