package optimisation

import optimisation.consonancemetrics.HarmonicityTF.graph
import optimisation.consonancemetrics.TFtester.{chordPlaceHolder, weightsPlaceHolder}
import org.tensorflow.{Output, Session}
import org.tensorflow.ndarray.{NdArrays, Shape, StdArrays}
import org.tensorflow.proto.framework.DataType
import org.tensorflow.types.{TFloat64, TInt32}
import org.tensorflow.types.family.TType

import scala.collection.JavaConverters._

object TensorFlowHelpers {

  def probe[T <: TType](output: Output[T]) = {

    val chord = Array(0,7)
    val weights = Array(1.0,1.0)

    val chordNDArray = NdArrays.ofInts(Shape.of(chord.size))
    StdArrays.copyTo(chord, chordNDArray)

    val weightsNDArray = NdArrays.ofDoubles(Shape.of(weights.size))
    StdArrays.copyTo(weights, weightsNDArray)

    val sess = new Session(graph)
    val res = sess.runner()
      .fetch(output)
      .feed(chordPlaceHolder, TInt32.tensorOf(chordNDArray))
      .feed(weightsPlaceHolder, TFloat64.tensorOf(weightsNDArray))
      .run().iterator().asScala.toList

    val scalaRes = res.map(t => {
      val tAsType = (t.dataType(), t.shape().asArray().size) match {
        case (DataType.DT_INT32, 1) => StdArrays.array1dCopyOf(t.asInstanceOf[TInt32])
        case (DataType.DT_DOUBLE, 1) => StdArrays.array1dCopyOf(t.asInstanceOf[TFloat64])
        case (DataType.DT_INT32, 2) => StdArrays.array2dCopyOf(t.asInstanceOf[TInt32]).map(_.toList)
        case (DataType.DT_DOUBLE, 2) => StdArrays.array2dCopyOf(t.asInstanceOf[TFloat64]).map(_.toList)
      }
      tAsType.toList
    })

    println(s"probe ${output.name()}: ${scalaRes.foreach(r => println(r))}")
  }
}
