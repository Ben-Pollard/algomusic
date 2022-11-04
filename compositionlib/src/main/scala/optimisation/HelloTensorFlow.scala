package optimisation

import breeze.stats.distributions.{Gaussian, RandBasis}
import midi.FLStudioMIDIMap
import optimisation.consonancemetrics.Harmonicity.getComplexTone
import org.tensorflow.framework.utils.CastHelper
import org.tensorflow.ndarray.StdArrays
import org.tensorflow.op.{OpScope, Ops}
import org.tensorflow.proto.framework.DataType
import org.tensorflow.types.{TFloat64, TInt32}
import org.tensorflow.{Graph, Session}

import scala.collection.JavaConverters._

object HelloTensorFlow extends App {

  val graph = new Graph()
  val root = new OpScope(graph)
  val tf = Ops.create(graph)

  val num_harmonics = 12
  val rho = 0.75
  val sigma = 6.83
  val array_dim = 1200
  val array_dim_const = tf.constant(array_dim)

  val pitchNames = List("C3", "B2")
  val chord = pitchNames.map(name => FLStudioMIDIMap.midiMap.get(name).get)
  val chromatic_degrees = chord.map(i => i % 12).toSet
  val weights = Array.fill(chromatic_degrees.size)(1.0)
  val spectra = (chord.toArray zip weights).map(x => getComplexTone(x._1, x._2))



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


//  def get_pc_spectrum_template_2(mean: Placeholder[TFloat64], mass: Placeholder[TFloat64]) = {
//
//    val origin = CastHelper.cast(tf, tf.math.round(mean), classOf[TInt32])
//    val scaled = tf.math.mul(pc_spectrum_template_1, mass)
//    val indexRange = tf.range(tf.constant(0), array_dim_const, tf.constant(1))
//    val x_minus_origin = tf.math.sub(indexRange.asOutput(), origin.asOutput())
//    val mod = tf.math.mod(x_minus_origin, array_dim_const)
//    val indices = mod
//    indices
//  }

  val meanPlaceHolder = tf.placeholder(classOf[TFloat64])
  val massPlaceHolder = tf.placeholder(classOf[TFloat64])

  val origin = CastHelper.cast(tf, tf.math.round(meanPlaceHolder), classOf[TInt32])
  val scaled = tf.math.mul(pc_spectrum_template_1, massPlaceHolder)
  val indexRange = tf.range(tf.constant(0), array_dim_const, tf.constant(1))
  val x_minus_origin = tf.math.sub(indexRange.asOutput(), origin.asOutput())
  val mod = tf.math.floorMod(x_minus_origin, array_dim_const)

  val mean = TFloat64.scalarOf(1.0)
  val mass = TFloat64.scalarOf(2.0)

  val sess = new Session(graph)
  val res = sess.runner()
    .fetch(indexRange.asOutput())
    .fetch(x_minus_origin.asOutput())
    .fetch(mod.asOutput())
    .feed(meanPlaceHolder, mean)
    .feed(massPlaceHolder, mass)
    .run().iterator().asScala.toList

  val scalaRes = res.map(t => {
    val tAsType = t.dataType() match {
      case DataType.DT_INT32 => StdArrays.array1dCopyOf(t.asInstanceOf[TInt32])
      case DataType.DT_DOUBLE => StdArrays.array1dCopyOf(t.asInstanceOf[TFloat64])
    }
    tAsType.toList
  })

  println(scalaRes.foreach(r => println(r)))




}
