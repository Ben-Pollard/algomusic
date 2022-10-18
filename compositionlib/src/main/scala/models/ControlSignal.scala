package models

import com.github.psambit9791.jdsp.signal.Generate


case class ControlSignal(s: Vector[Double]) {

  def scaleToByte() = {
    val (min, max) = (s.min, s.max)
    s.map(i => (127 * ((i - min) / (max - min))).toInt)
  }

  def scaleToRange(minTo: Int, maxTo: Int): Vector[Int] = {
    val (min, max) = (s.min, s.max)
    s.map(i => (minTo + ((maxTo - minTo) * ((i - min) / (max - min)))).toInt)
  }

  def getTimeIndex() = {
    (0 until s.size).toVector
  }
}


