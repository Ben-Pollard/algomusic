package transformers

import models.Bar.ParallelBarSequences
import models.BarSequence

object ArrangementTransformers {

  def repeatArrangement(arrangement: ParallelBarSequences, n: Int): ParallelBarSequences = {
    arrangement.map(barSequence => {
      BarSequence(List.fill(n)(barSequence.bars).flatten, barSequence.channel)
    })
  }

}
