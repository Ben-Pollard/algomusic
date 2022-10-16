package transformers

import models.{AbstractBar, Bar}

trait BarTransformers extends AbstractBar {
  def +(bar: Bar): Bar = {
    assert((voices ++ bar.voices).map(_.map(_.duration).sum).toSet.size == 1)
    assert(instrument == bar.instrument)
    Bar(voices ++ bar.voices, instrument)
  }
}
