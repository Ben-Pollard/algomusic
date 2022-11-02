package optimisation

import breeze.linalg.{DenseMatrix, DenseVector}
import models.Primitives.MidiPitch
import optimisation.consonancemetrics.DissonanceCurve.getDissonanceOfPitches

object HarmonicArray {
  // For a given note, there will be an array of dissonances
  val intervalDissonances = DenseMatrix.tabulate(128,128){
    case (i, j) => getDissonanceOfPitches(List(i, j))
  }


}
