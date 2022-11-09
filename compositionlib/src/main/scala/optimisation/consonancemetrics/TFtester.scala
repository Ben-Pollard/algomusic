package optimisation.consonancemetrics

import breeze.linalg.DenseMatrix
import optimisation.consonancemetrics.HarmonicityTF.getGraph
import org.tensorflow.ndarray.{NdArrays, Shape, StdArrays}
import org.tensorflow.types.{TFloat64, TInt32}
import org.tensorflow.{Graph, Session}

object TFtester extends App {

  val (graph, harmonicity, chordPlaceHolder, weightsPlaceHolder) = getGraph()
  val sess = new Session(graph)


  def getHarmonicity(s: Session, g: Graph, chord: Array[Int], weights: Array[Double]) = {
    val chordNDArray = NdArrays.ofInts(Shape.of(chord.size))
    StdArrays.copyTo(chord, chordNDArray)

    val weightsNDArray = NdArrays.ofDoubles(Shape.of(weights.size))
    StdArrays.copyTo(weights, weightsNDArray)

    val resultTensor = s
      .runner()
      .fetch(harmonicity)
      .feed(chordPlaceHolder, TInt32.tensorOf(chordNDArray))
      .feed(weightsPlaceHolder, TFloat64.tensorOf(weightsNDArray))
      .run().get(0)

    val result = StdArrays.array1dCopyOf(resultTensor.asInstanceOf[TFloat64])(0)
    result
  }


//  val pitches = pitchNames.map(name => FLStudioMIDIMap.midiMap.get(name).get
  println(getHarmonicity(sess, graph, Array(0,7), Array(1.0,1.0)))
  println(Harmonicity.getHarmonicity(List(0,7)))

  val tfStart = System.currentTimeMillis()
  val x = DenseMatrix.tabulate(127,127){
    case (i, j) => getHarmonicity(sess, graph, Array(i, j), Array(1.0,1.0))
  }
  sess.close()
  graph.close()
  val tfEnd = System.currentTimeMillis()

  val scalaStart = System.currentTimeMillis()
  val y = DenseMatrix.tabulate(5,5){
    case (i, j) => Harmonicity.getHarmonicity(List(i, j))
  }
  val scalaEnd = System.currentTimeMillis()

  println(s"TF Time: ${(tfEnd - tfStart) / 1000}s")
  println(s"Scala Time: ${(scalaEnd - scalaStart) / 1000}s")

}
