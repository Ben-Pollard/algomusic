import breeze.linalg.DenseMatrix
import dev.ludovic.netlib.blas.{BLAS, NativeBLAS}
import midi.FLStudioMIDIMap
import models.Primitives.midiRange
import optimisation.EqualTemperament._
import optimisation.HarmonicArray
import optimisation.consonancemetrics.CombinedConsonance.getConsonanceOfPitches
import optimisation.consonancemetrics.DissonanceCurve.getDissonanceOfPitches


val y = HarmonicArray.intervalDissonances

//val x = DenseMatrix.tabulate(128,128){
//  case (i, j) => getConsonanceOfPitches(List(i, j))
//}

val x = DenseMatrix.tabulate(2,2){
  case (i, j) => getConsonanceOfPitches(List(i, j))
}

FLStudioMIDIMap.midiMap.get("C3").get
FLStudioMIDIMap.midiMap.get("G3").get

pitchName2Freq("C3")
pitchName2Freq("G3")

pitchNum2Freq(36)
pitchNum2Freq(43)
pitchNum2Freq(127)
val ratio = pitchNum2Freq(127) / pitchNum2Freq(36)

getDissonanceOfPitches(List(36,43))
getDissonanceOfPitches(List(36,127))

// can we pick out the most consonant note for a given note?
(y(::, 36).toScalaVector zip midiRange).sortBy(_._1).map(_._2)
