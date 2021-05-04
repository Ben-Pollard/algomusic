package models


case class Arrangement(barSequences: List[BarSequence]) {

  def ++ (arrangement: Arrangement) = {
    Arrangement(barSequences ++ arrangement.barSequences)
  }

  def repeat(n: Int): Arrangement = {
    Arrangement(barSequences.map(barSequence => {
      BarSequence(List.fill(n)(barSequence.bars).flatten, barSequence.channel)
    }))
  }

}
