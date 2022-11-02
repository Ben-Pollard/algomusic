package optimisation.consonancemetrics

import breeze.linalg.{Axis, DenseMatrix, DenseVector, sum}
import breeze.stats.distributions.{Gaussian, RandBasis}
import midi.FLStudioMIDIMap
import models.Primitives.PitchName

object Harmonicity {
  val log2 = (x: Double) => math.log10(x) / math.log10(2.0)

  val num_harmonics = 12
  val rho = 0.75
  val sigma = 6.83
  val array_dim = 1200

  def getComplexTone(pc: Int, weight: Double) = {
    val pcs = (1 to num_harmonics).map(i => {
      ((pc * array_dim / 12) + (array_dim * log2(i))) % array_dim
    })

    val weights = (1 to num_harmonics).map(i => {
      weight / math.pow(i, rho)
    })

    val spectra = (pcs zip weights).map(x => {
      val (pc, weight) = x
      pc_spectrum_template_2(array_dim, pc, weight, sigma).toArray
    }).toArray

    val m = new DenseMatrix(array_dim, spectra.length, spectra.flatten)

    sum(m, Axis._1)

  }

  def pc_spectrum_template_2(array_dim: Int, mean: Double, mass: Double, sigma: Double, truncation_point: Int = 12) = {
    val origin = math.round(mean)
    val template = pc_spectrum_template_1(array_dim, sigma, truncation_point)
    val scaled = template.map(_ * mass)
    val indices = (0 until array_dim).map(x => {
      val x_minus_origin = x - origin
      val i = if (x_minus_origin >= 0) x_minus_origin % array_dim else (array_dim - origin + x) % array_dim
      i
    })
    val output = indices.map(i => scaled(i.toInt))
    //output <- scaled[((seq - origin) %% array_dim) + 1]
    output
  }

  def pc_spectrum_template_1(array_dim: Int, sigma: Double, truncation_point: Int) = {
    val gaussian = Gaussian(0, 6.83)(RandBasis.mt0)
    val limit = math.floor(sigma * 12).toInt
    val template = (0 until array_dim).map(i => {
      i match {
        case x if x <= limit => gaussian.pdf(i)
        case x if x >= array_dim - limit => gaussian.pdf(array_dim - x)
        case _ => 0.0
      }
    })
    template
  }

  def get_milne_pc_spectrum(chord: Set[Int], weights: Array[Double]) = {
    val spectra = (chord.toArray zip weights).map(x => getComplexTone(x._1, x._2))
    val milne_pc_spectrum = sum(DenseMatrix(spectra: _*), Axis._0)
    milne_pc_spectrum
  }

  def sweep_template(x: Vector[Double], template: DenseVector[Double]) = {
    (0 until x.length).map(i => {
      val (first, last) = x.splitAt(i)
      1 - breeze.linalg.cosineDistance(template, DenseVector((last ++ first).toArray))
    })
  }

  def getHarmonicity(chord: List[Int]) = {
    val x = chord.map(i => i % 12).toSet
    val weights = Array.fill(x.size)(1.0)

    val milne_pc_spectrum = get_milne_pc_spectrum(x, weights)
    val template = get_milne_pc_spectrum(Set(0), Array(1.0))
    val y = sweep_template(milne_pc_spectrum.inner.toScalaVector, template.t)
    val sum_y = y.sum
    val probs = y.map(x => x / sum_y)
    val uniform_probs = 1.0 / probs.size
    val nonzero_probs = probs.filter(_ > 0.0)
    val harmonicity = nonzero_probs.map(p => p * log2(p / uniform_probs)).sum
    harmonicity
  }

  def getHarmonicityOfPitchNames(pitchNames: List[PitchName]) = {
    getHarmonicity(pitchNames.map(name => FLStudioMIDIMap.midiMap.get(name).get))
  }

}
