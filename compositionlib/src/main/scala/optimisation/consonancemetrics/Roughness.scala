package optimisation.consonancemetrics

import midi.FLStudioMIDIMap
import models.Primitives.{Frequency, MidiPitch, PitchName}
import optimisation.EqualTemperament.{A440_Frequency, A440_Name, ratio}
import optimisation.Spectrum

object Roughness {
  val log2 = (x: Double) => math.log10(x) / math.log10(2.0)

  val A440_Pitch: MidiPitch = FLStudioMIDIMap.midiMap.get(A440_Name).get

  val numHarmonics = 11
  val rollOff = 1.0

  def hutch_cbw(f1: Array[Frequency], f2: Array[Frequency]) = {
    val mean_f = (f1 zip f2).map(f => (f._1 + f._2) / 2.0)
    mean_f.map(f => 1.72 * math.pow(f, 0.65))
  }

  def hutch_y(f1: Array[Frequency], f2: Array[Frequency]): Array[Double] = {
    val abs_freq_diff = (f1 zip f2).map(f => math.abs(f._1 - f._2))
    val critical_bandwidth = hutch_cbw(f1, f2)
    (abs_freq_diff zip critical_bandwidth).map(x => x._1 / x._2)
  }

  def hutch_g(y: Array[Double], cbw_cut_off: Double = 1.2, a: Double = 0.25, b: Double = 2): Array[Double] = {
    y.map(i => {
      if (i > cbw_cut_off) 0 else {
        math.pow((i / a) * math.exp(1 - (i / a)), b)
      }
    })
  }

  def hutch_dissonance_function(f1: Array[Frequency], f2: Array[Frequency], cbw_cut_off: Double = 1.2, a: Double = 0.25, b: Double = 2) = {
    hutch_g(y = hutch_y(f1 = f1, f2 = f2), cbw_cut_off = cbw_cut_off, a = a, b = b)
  }

  def roughness_hutch(x: Spectrum, cbw_cut_off: Double = 1.2, a: Double = 0.25, b: Double = 2) = {
    val frequency = x.frequencies
    val amplitude = x.amplitudes
    val n = frequency.length
    if (n < 2)
      0
    else {
      val denominator = amplitude.map(math.pow(_, 2)).sum
      val arrays = (0 until n).flatMap(i => (0 until n).filter(j => i < j)
        .map(j => {
          val amp_ij = amplitude(i) * amplitude(j)
          val f1 = frequency(i)
          val f2 = frequency(j)
          (i, j, amp_ij, f1, f2)
        }))

      val g_ij = hutch_dissonance_function(arrays.map(_._4).toArray, arrays.map(_._5).toArray)

      (arrays.map(_._3) zip g_ij).map(x => x._1 * x._2).sum / denominator

    }
  }

  def getRoughnessOfPitches(chord: List[MidiPitch]) = {

    val n = 1 to numHarmonics
    val interval = n.map(n => 12.0 * log2(n))
    val amplitude = n.map(n => 1.0 / math.pow(n, rollOff))

    val (freq, amp) = chord.flatMap(f = pitch => {
      val frequencies = interval.map(_ + pitch).map(p => A440_Frequency * Math.pow(ratio, p - A440_Pitch))
      val amplitudes = amplitude
      frequencies zip amplitudes
    }).sortBy(_._1).unzip

    //todo there is a competing spectrum builder
    val spectrum = new Spectrum(freq.toArray, amp.toArray)

    val roughness = roughness_hutch(spectrum)
    roughness
  }


  def getRoughnessOfPitchNames(pitchNames: List[PitchName]) = {
    getRoughnessOfPitches(pitchNames.map(name => FLStudioMIDIMap.midiMap.get(name).get))
  }


}
