package optimisation.consonancemetrics

import models.Primitives.{MidiPitch, PitchName}
import optimisation.Spectrum

object DissonanceCurve {

  def dissmeasure(spectrum: Spectrum) = {
    // given a set of partials in fvec,
    // with amplitudes in amp,
    // this routine calculates the dissonance

    val (fvec, amp) = (spectrum.frequencies, spectrum.amplitudes)
    val Dstar = 0.24
    val S1 = 0.0207
    val S2 = 18.96
    val C1 = 5
    val C2 = -5
    val A1 = -3.51
    val A2 = -5.75
    val firstpass = 1
    val N = fvec.length
    val (fvecSorted, ams) = fvec.zip(amp).sortBy(_._1).unzip

    (for (i <- 2 to N) yield {
      val Fmin = fvecSorted.slice(0, N - i + 1)
      val S = Fmin.map(f => Dstar / (S1 * f + S2))
      val Fdif = (fvecSorted.slice(i - 1, N) zip fvecSorted.slice(0, N - i + 1)).map(x => x._1 - x._2)
      val a = (ams.slice(i - 1, N) zip ams.slice(0, N - i + 1)).map(x => Array(x._1, x._2).min)
      val Dnew = Fdif.zip(S).map(x => {
        val (f, s) = x
        C1 * math.exp(A1 * s * f) + C2 * math.exp(A2 * s * f)
      })
        .zip(a).map(x => x._1 * x._2)
      Dnew.sum
    }).reduce(_ + _)
  }

  def getDissonanceOfPitchNames(notes: List[PitchName]) = {
    val spectrum = Spectrum.getSpectrumFromPitchNames(notes)
    dissmeasure(spectrum)
  }

  def getDissonanceOfPitches(pitches: List[MidiPitch]) = {
    val spectrum = Spectrum.getSpectrumFromPitchNumbers(pitches)
    dissmeasure(spectrum)
  }

}
