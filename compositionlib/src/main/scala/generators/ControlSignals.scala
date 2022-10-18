package generators

import com.github.psambit9791.jdsp.signal.Generate
import models.ControlSignal

object ControlSignals {

 def generateSineWave(signalLength: Int): ControlSignal = {
  val signal = new Generate(0, signalLength, signalLength)
    .generateSineWave(1)
    .toVector
  assert(signal.size == signalLength)

  ControlSignal(signal)
 }

}