import optimisation.consonancemetrics.DissonanceCurve.{dissmeasure, getDissonanceOfPitchNames}
import plotly._
import Plotly._
import optimisation.consonancemetrics.Roughness.getRoughnessOfPitchNames
import optimisation.consonancemetrics.Harmonicity.getHarmonicityOfPitchNames
import optimisation.Spectrum
import optimisation.consonancemetrics.CombinedConsonance.getConsonanceOfPitchNames

val freq = (1 to 15).map(_*440.0).toArray
val amp = Array.fill(freq.size)(1.0)
val range = 10
val inc=0.01

val x = (BigDecimal(1) to BigDecimal(range) by(inc)).map(_.toDouble)

val y = x.map(alpha => {
  val f = freq ++ freq.map(_*alpha)
  val a = amp ++ amp
  dissmeasure(Spectrum(f, a))
})


Scatter(x, y).plot(path = "D:\\temp\\temp.html")


// the velocity of each note in the harmonic is important to the dissonance
// and that means we need some model of velocity -> amplitude
// that would be best derived empirically per-patch.
// there is published work suggesting that a square-root function is popular

getDissonanceOfPitchNames(List("C3", "B2"))
getDissonanceOfPitchNames(List("C3", "E3"))
getDissonanceOfPitchNames(List("C3", "G3"))
getDissonanceOfPitchNames(List("C3", "E3", "G3"))

getRoughnessOfPitchNames(List("C3", "B2"))
getRoughnessOfPitchNames(List("C3", "E3"))
getRoughnessOfPitchNames(List("C3", "G3"))
getRoughnessOfPitchNames(List("C3", "E3", "G3"))

getHarmonicityOfPitchNames(List("C3", "B2"))
getHarmonicityOfPitchNames(List("C3", "E3"))
getHarmonicityOfPitchNames(List("C3", "G3"))
getHarmonicityOfPitchNames(List("C3", "E3", "G3"))

getConsonanceOfPitchNames(List("C3", "B2"))
getConsonanceOfPitchNames(List("C3", "E3"))
getConsonanceOfPitchNames(List("C3", "G3"))
getConsonanceOfPitchNames(List("C3", "E3", "G3"))