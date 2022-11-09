package optimisation.consonancemetrics

import breeze.stats.distributions.{Gaussian, RandBasis}
import org.tensorflow.framework.utils.CastHelper
import org.tensorflow.op.{OpScope, Ops}
import org.tensorflow.types.{TFloat64, TInt32}
import org.tensorflow.{Graph, Operand}
import scala.collection.JavaConverters._

object HarmonicityTF {

  val graph = new Graph()
  val root = new OpScope(graph)
  val tf = Ops.create(graph)

  val num_harmonics = 12
  val num_harmonics_const = tf.constant(num_harmonics)
  val rho = tf.constant(0.75)
  val sigma = 6.83
  val array_dim = 1200
  val array_dim_const = tf.constant(array_dim)
  val array_dim_const_double = tf.constant(array_dim.toDouble)
  val const0 = tf.constant(0)
  val const1 = tf.constant(1)
  val const2 = tf.constant(2)
  val const2Double = tf.constant(2.0)
  val const12Double = tf.constant(12.0)
  val log2tf = (x: Operand[TFloat64]) => tf.math.div(tf.math.log(x), tf.math.log(const2Double))


  def get_pc_spectrum_template_1(array_dim: Int, sigma: Double, truncation_point: Int = -1) = {
    val gaussian = Gaussian(0, 6.83)(RandBasis.mt0)
    val limit = math.floor(sigma * 12).toInt
    val templateValues = (0 until array_dim).map(i => {
      i match {
        case x if x <= limit => gaussian.pdf(i)
        case x if x >= array_dim - limit => gaussian.pdf(array_dim - x)
        case _ => 0.0
      }
    }).toArray
    tf.constant(templateValues)
  }

  lazy val pc_spectrum_template_1 = get_pc_spectrum_template_1(array_dim, sigma)

  def get_pc_spectrum_template_2(mean: Operand[TFloat64], mass: Operand[TFloat64], chordSize: Operand[TInt32]) = {

    val scaled = tf.math.mul(
      tf.reshape(pc_spectrum_template_1, tf.constant(Array(1,1,-1))),
      tf.expandDims(mass, const2)
    )

    val indexRange = tf.range(const0, array_dim_const, const1)

    val origin = CastHelper.cast(tf, tf.math.round(mean), classOf[TInt32])

    val x_minus_origin = tf.math.sub(
      tf.reshape(indexRange, tf.constant(Array(1,1,-1))).asOutput(),
      tf.expandDims(origin, const2).asOutput()
    )
    val indices = tf.math.floorMod(x_minus_origin, array_dim_const)

    val spectralBandIndices = tf.reshape(
      indices,
      tf.constant(Array(-1))).asOutput()

    val harmonicIndices = tf.tile(
      tf.reshape(
        tf.linalg.transpose(
          tf.reshape(
            tf.tile(
              tf.range(const0, num_harmonics_const, const1),
              tf.constant(Array(array_dim))
            ), tf.constant(Array(array_dim,-1))
          ),
          tf.constant(Array(1,0))
        ),
        tf.constant(Array(-1))
      ),
      tf.expandDims(chordSize, const0)
    )

    val chordIndices = tf.reshape(
      tf.linalg.transpose(
        tf.reshape(
          tf.tile(
            tf.range(const0, chordSize, const1),
            tf.constant(Array(array_dim*num_harmonics))
          ),
          tf.constant(Array(array_dim*num_harmonics, -1))
        ),
        tf.constant(Array(1,0))
      ),
      tf.constant(Array(-1))
    )

    val gatherIndices = tf.linalg.transpose(
      tf.stack(Iterable(chordIndices, harmonicIndices, spectralBandIndices).asJava),
      tf.constant(Array(1,0))
    )

    val gathered = tf.reshape(
      tf.gatherNd(scaled, gatherIndices),
      tf.concat(Iterable(tf.reshape(chordSize, tf.constant(Array(1))).asOutput(), tf.constant(Array(num_harmonics,array_dim))).asJava, const0)
    )

    gathered
  }


  def getComplexTone(tone: Operand[TInt32], weight: Operand[TFloat64]) = {
    val harmonicIndices = tf.math.add(tf.range(const0, num_harmonics_const, const1), const1)
    val harmonicIndicesDouble = CastHelper.cast(tf, harmonicIndices, classOf[TFloat64])
    val dimMulIndex = tf.math.mul(array_dim_const_double, log2tf(harmonicIndicesDouble))

    val toneAsDouble = CastHelper.cast(tf, tone, classOf[TFloat64])
    val dimMulTone = tf.math.mul(toneAsDouble, tf.math.div(array_dim_const_double, const12Double))

    val pcs = tf.math.mod(
      tf.math.add(
        tf.expandDims(dimMulTone, const1),
        tf.expandDims(dimMulIndex, const0)
      ),array_dim_const_double)

    val weights = tf.math.div(
      tf.expandDims(weight,const1),
      tf.expandDims(tf.math.pow(harmonicIndicesDouble, rho), const0)
    )

    val pc_spectrum_template_2 = get_pc_spectrum_template_2(pcs, weights, tf.size(tone))
    tf.reduceSum(pc_spectrum_template_2, const1)
  }

  def get_milne_pc_spectrum(chord: Operand[TInt32], weights: Operand[TFloat64]) = {
    val spectra = getComplexTone(chord, weights)
    val milne_pc_spectrum = tf.reduceSum(spectra, const0)
    milne_pc_spectrum
  }

  def sweep_template(milne_pc_spectrum: Operand[TFloat64], template: Operand[TFloat64]) = {
    val indices = tf.range(const0, array_dim_const, const1)

    val colIndices = tf.reshape(
      tf.math.mod(
        tf.math.add(
          tf.reshape(
            tf.tile(
              indices,
              tf.constant(Array(array_dim))
            ),
            tf.constant(Array(array_dim,-1))
          ),
          tf.expandDims(indices, const1)
        ),
        array_dim_const
      ),
      tf.constant(Array(-1))
    )

    val rowIndices = tf.reshape(
      tf.linalg.transpose(
        tf.reshape(
          tf.tile(indices, tf.constant(Array(array_dim))),
          tf.constant(Array(array_dim,-1))
        ),
        tf.constant(Array(1,0))
      ),
      tf.constant(Array(-1))
    )

    val matrixTemplate = tf.reshape(
        tf.tile(milne_pc_spectrum, tf.constant(Array(array_dim))),
        tf.constant(Array(array_dim,-1))
      )

    val matrix = tf.reshape(
      tf.gatherNd(
        matrixTemplate,
        tf.linalg.transpose(
          tf.stack(Iterable(rowIndices, colIndices.asOutput()).asJava),
          tf.constant(Array(1,0))
        )
      ),
      tf.constant(Array(array_dim,array_dim))
    )

    val matrixNorm = tf.math.pow(
      tf.linalg.einsum(Iterable(matrix, matrix.asOutput()).asJava, "ij,ij->i").asOutput(),
      tf.constant(0.5)
    )

    val vectorNorm = tf.math.pow(
      tf.linalg.einsum(Iterable(template, template.asOutput()).asJava, "i,i->"),
      tf.constant(0.5)
    )

    val dot = tf.linalg.einsum(Iterable(matrix.asOutput(), template).asJava, "ij,i->j")

    val cosineSimilarity = tf.math.div(
      dot,
      tf.math.mul(
        matrixNorm,
        vectorNorm
      )
    )

    cosineSimilarity
  }

  def calcHarmonicity(chord: Operand[TInt32], weights: Operand[TFloat64]) = {

    val milne_pc_spectrum = get_milne_pc_spectrum(chord, weights)
    val template = get_milne_pc_spectrum(tf.constant(Array(0)), tf.constant(Array(1.0)))
    val y = sweep_template(milne_pc_spectrum, template)
    val sum_y = tf.sum(y, const0)
    val probs = tf.math.div(y, sum_y)

    val uniform_probs = tf.math.div(
      tf.constant(1.0),
      CastHelper.cast(tf, tf.size(probs), classOf[TFloat64])
    )
    val mask = tf.math.greater(probs, tf.constant(0.0))
    val nonzero_probs = tf.booleanMask(probs, mask)
    val harmonicity = tf.sum(tf.math.mul(
      nonzero_probs,
      log2tf(tf.math.div(nonzero_probs, uniform_probs))
    ), const0)

    tf.expandDims(harmonicity, const0)
  }

  def getGraph() = {
    val chordPlaceHolder = tf.placeholder(classOf[TInt32])
    val weightsPlaceHolder = tf.placeholder(classOf[TFloat64])
    val harmonicity = calcHarmonicity(chordPlaceHolder, weightsPlaceHolder)
    (graph, harmonicity, chordPlaceHolder, weightsPlaceHolder)
  }

}
