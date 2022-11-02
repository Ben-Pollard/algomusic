package optimisation

import models.Primitives.{Amplitude, Frequency, MidiNote, MidiPitch, PitchName, Velocity}
import optimisation.EqualTemperament.{pitchName2Freq, pitchNum2Freq}

case class Spectrum(frequencies: Array[Frequency], amplitudes: Array[Amplitude])

object Spectrum {
  // Constructors create an implied spectrum based on midi notes
  def apply(fundamentals: Array[Frequency], fundamentalAmplitudes: Array[Amplitude]): Spectrum = {
    val harmonics = 0 to 7 //NB check results with different harmonic ranges - interference will disappear if too narrow
    val harmonicAmplitudes = harmonics.map(i => math.exp(-0.25 * i)) // NB static roll-off function. May be better to either not roll-off or to vary by instrument
    val frequencies = fundamentals.flatMap(f => harmonics.map(_*f))
    val amplitudes = (fundamentals zip fundamentalAmplitudes).flatMap(fa => harmonicAmplitudes.map(_ * fa._2))
    val (uniqueFreqs, summedAmplitudes) = (frequencies zip amplitudes)
      .groupBy(_._1).mapValues(fa => {
      val frequency = fa.head._1
      val amplitude = fa.map(_._2).sum
      (frequency, amplitude)
    }).values.unzip
    new Spectrum(uniqueFreqs.toArray, summedAmplitudes.toArray)
  }

  def apply(fundamentals: Array[Frequency]): Spectrum = {
    apply(fundamentals = fundamentals, fundamentalAmplitudes = Array.fill(fundamentals.length)(1.0))
  }

  def getSpectrumFromPitchNumbers(midiPitches: Seq[MidiPitch]): Spectrum = {
    val fundamentals = midiPitches.map(n => pitchNum2Freq(n)).toArray
    apply(fundamentals = fundamentals)
  }

  def getSpectrumFromPitchNames(pitchNames: Seq[PitchName]): Spectrum = {
    val fundamentals: Array[Frequency] = pitchNames.map(n => pitchName2Freq.get(n).get).toArray
    apply(fundamentals)
  }
  def getSpectrumFromMidiNotes(midinotes: Seq[(MidiPitch, Velocity)]): Spectrum = {
    val (midiPitches, velocities) = midinotes.unzip
    val fundamentals: Array[Frequency] = midiPitches.map(n => pitchNum2Freq(n)).toArray
    val fundamentalAmplitudes: Array[Amplitude] = velocities.map(_.toDouble).toArray //NB this might be best represented as a power function of the velocity, depending on synth
    apply(fundamentals = fundamentals, fundamentalAmplitudes = fundamentalAmplitudes)
  }




}