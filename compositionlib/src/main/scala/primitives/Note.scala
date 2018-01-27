package primitives

class Duration()

class Note(midiVal:Int, duration:Duration)

class Mode(intervalPattern: Vector[Int]) {
  def degrees2Notes(degrees: Vector[Int]): Vector[Note] = {

  }
}

class Scale(tonic: Note, mode: Mode)

