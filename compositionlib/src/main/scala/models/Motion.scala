package models

import models.Primitives.{DOWN, Direction, NO_DIRECTION, OverloadedClassIndicator, ScaleDegree, UP}



case class Motion(direction: Direction, degreeA: ScaleDegree, degreeB: ScaleDegree, overloaded: OverloadedClassIndicator)

object Motion {
  def apply(direction: Direction, startDegree: ScaleDegree, degreeStep: Int): Motion = {
    val degreeB = direction match {
      case UP => startDegree + degreeStep
      case DOWN => startDegree - degreeStep
      case NO_DIRECTION => startDegree
    }
    new Motion(direction = direction, degreeA = startDegree, degreeB = degreeB, 1.asInstanceOf[OverloadedClassIndicator])
  }
}