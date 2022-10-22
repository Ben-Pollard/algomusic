import breeze.linalg.{DenseMatrix, DenseVector}
import models.Primitives.{h, q, w}
import util.Util.lowestCommonMultiple

import scala.collection.immutable.List

val notesPerBar = 5
val chordChangeEach = 4
//val velocityPattern = 6

val durationPattern: List[Double] = List(w, h, q, w, h)
val velocityPattern: List[Double] = List(100, 75, 65, 90, 75, 85)

val lcm = lowestCommonMultiple(List(notesPerBar,chordChangeEach,durationPattern.size, velocityPattern.size))


val barIndex = (for (i <- 1 to lcm/notesPerBar) yield {
  for (j <- 1 to notesPerBar) yield i.toDouble
}).flatten.toArray

val chordIndex = (for (i <- 1 to lcm/chordChangeEach) yield {
   for (j <- 1 to chordChangeEach) yield i.toDouble
}).flatten.toArray

val velocities = (for (i <- 1 to lcm/velocityPattern.size) yield {
  velocityPattern
}).flatten.toArray

val durations = (for (i <- 1 to lcm/durationPattern.size) yield {
  durationPattern
}).flatten.toArray

val driverMatrix = DenseMatrix(barIndex, chordIndex, durations, velocities).t

val barIndex = 2.0
val slicer = driverMatrix(::,0) :== barIndex
driverMatrix(slicer, ::)

val nullmatrix = DenseMatrix(List(0.0)).t
val nullslicer = nullmatrix(::,0) :== 0.0
val nullslicematrix = nullmatrix(nullslicer, ::)