package music.project1

import breeze.linalg.DenseMatrix
import generators.RhythmGenerators.generateBjorklundSequence
import instruments.CC_BBCSO
import models.Primitives.{h, q, w}
import models.barconstructors.PolyphonicScalePhraseBarConstructor
import models.midibuilders.ArrangementConstruction.BarInfo
import models.{PolyphonicScalePhrase, Scale, ScalePhrase}
import util.Util.lowestCommonMultiple


object BaseHarmonyFromControlMatrix {
  def apply(scale: Scale) = {

    val chordRoots = Array(1, 3 - 8, 6 - 8, 7 - 8, 5 - 8).map(_ - 1)
    val chordDegrees = List(1, 3, 5).map(_ - 1)

    val notesPerBar = 5
    val chordChangeEach = 4
    val durationPattern: List[Double] = List(w, h, q, w, h)
    val velocityPattern: List[Double] = List(100, 75, 65, 90, 75, 85)
    val lcm = lowestCommonMultiple(List(notesPerBar,chordChangeEach,durationPattern.size, velocityPattern.size))

    val barIndex = (for (i <- 0 until lcm/notesPerBar) yield {
      for (j <- 1 to notesPerBar) yield i.toDouble
    }).flatten.toArray

    val chordIndex = (for (i <- 0 until lcm/chordChangeEach) yield {
      for (j <- 1 to chordChangeEach) yield i.toDouble
    }).flatten.toArray

    val velocities = (for (i <- 1 to lcm/velocityPattern.size) yield {
      velocityPattern
    }).flatten.toArray

    val durations = (for (i <- 1 to lcm/durationPattern.size) yield {
      durationPattern
    }).flatten.toArray

    val driverMatrix = DenseMatrix(barIndex, chordIndex, durations, velocities).t

    for (i <- 0 until barIndex.max.toInt) yield {
      val barIndex = i
      val slicer = driverMatrix(::,0) :== barIndex.toDouble
      val slice = driverMatrix(slicer, ::)
      val chordIndices = slice(::,1).toScalaVector.map(_.toInt).toList
      val durations = slice(::,2).toScalaVector
      val velocities = slice(::,3).toScalaVector.map(_.toInt)

      val rhythm = generateBjorklundSequence(
        16,
        notesPerBar,
        hitDurations = durations,
        velocities = velocities
      )
        .steps2Subdivisions(4)

      val pp = chordDegrees.map(degree => {
        val notes = chordIndices.map(i => chordRoots(i % chordRoots.length) + degree)
        ScalePhrase(notes, scale)
      })

      val barConstructor = PolyphonicScalePhraseBarConstructor(PolyphonicScalePhrase(pp), rhythm)
        .applySineToEachNote(CC_BBCSO.expression.id)

      BarInfo(barConstructor, slice)
    }


  }
}
