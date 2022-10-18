import com.github.psambit9791.jdsp.signal.Generate
import models.ControlSignal
import plotly.Plotly.TraceOps

// todo investigate using adsr parameters
// step 1: plot log function

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

object ControlSignal {

  def apply(signalLength: Int): ControlSignal = {
    val signal = new Generate(0, 1, signalLength)
//      .generateSineWave(1)
      .generateRicker(1, 1.0)
      .toVector
    assert(signal.size == signalLength)

    ControlSignal(signal)
  }

}

val signal = ControlSignal(100)

plotly
  .Bar(signal.getTimeIndex, signal.scaleToByte)
  .plot(path = "D:\\temp\\plot.html", openInBrowser=true)

