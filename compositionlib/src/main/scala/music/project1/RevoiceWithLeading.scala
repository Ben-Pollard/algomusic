package music.project1

import enums.VoicingQualities
import models._
import models.barconstructors.PolyphonicScalePhraseBarConstructor
import models.midibuilders.ArrangementConstruction.{BarConstructionAndSequencingData, BarInfo}

object RevoiceWithLeading {

  def apply(scale: Scale, baseHarmony: Seq[BarInfo[PolyphonicScalePhraseBarConstructor]]): BarConstructionAndSequencingData = {

    val revoiced: BarConstructionAndSequencingData = baseHarmony.map(p => {
      val newConstructor = p.constructor
        .revoice(
          staticRoot=false,
          allowInversions=true,
          voicingQuality = VoicingQualities.INTERNALLY_SPICY,
          qualityRank = 0,
          scale = scale
        )
        .leading()

      p.copy(constructor = newConstructor)
    }).toList

    revoiced
  }

}
