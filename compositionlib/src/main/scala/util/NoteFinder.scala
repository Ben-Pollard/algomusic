package util

import models.Primitives._
import models.Scale

object NoteFinder {

  def stepDegrees(direction: Direction, startDegree: ScaleDegree, degreeStep: Int): ScaleDegree = {
    direction match {
      case UP => startDegree + degreeStep
      case DOWN => startDegree - degreeStep
      case NO_DIRECTION => startDegree
    }
  }

  def stepToDegree(direction: Direction, directFrom: ScaleDegree, degreesFrom: ScaleDegree, degreeDiff: Int, scale: Scale): ScaleDegree = {
    val targetDegree = degreesFrom + degreeDiff
    val possibleDegrees = (-10 to 10).map(i => targetDegree + (i * scale.pattern.size))

    direction match {
      case UP => possibleDegrees.filter(_ > directFrom).min
      case DOWN => possibleDegrees.filter(_ < directFrom).max
      case NO_DIRECTION => possibleDegrees.map(t => (t, (t - directFrom).abs)).sortBy(_._2).head._1
    }
  }

}
