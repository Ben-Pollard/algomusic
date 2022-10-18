package models

case class PolyphonicScalePhrase(phrases: List[ScalePhrase]) {
  def transpose(transposeDegrees: Int): PolyphonicScalePhrase = {
    PolyphonicScalePhrase(phrases.map(_.transpose(transposeDegrees)))
  }
}
