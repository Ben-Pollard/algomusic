import midi.PatternPlayer
import org.jfugue.pattern.Pattern
import org.jfugue.theory.ChordProgression
import primitives.Mappings._
import primitives.Primitives._

//val p = new ChordProgression("|I ii iii IV V VI vii")
//  .setKey("C")
//    .getPattern.toString
//
//
//val tempo = "T120 "
//val channel = "v0 " //v9 is percussion
//
//PatternPlayer(new Pattern(tempo + channel + p))

//We can request a scale degree from 1 to 14
//The object returned is an index to the scale
//The scale consists of a series of degrees and pitches
 //offsets
val root = 60
majorScalePattern
val offsets = majorScalePattern.scanLeft(0)(_+_)
val degreeMap =
  Seq.fill(2)(offsets.take(7) map(_+root))
  .zipWithIndex
  .flatMap(s => s._1 map(_+s._2*12))
  .zipWithIndex map(p => (p._2 + 1, p._1)) toMap


val pitches = Seq.fill(10)(majorScalePattern.scanRight(0)(_+_).takeRight(7).reverse)
  .zipWithIndex
  .flatMap(s => s._1 map(i => root - (i + s._2*12) ))
  .reverse
  .filter(n => n>=0 & n<root) ++
  Seq.fill(10)(offsets.take(7) map(_+root))
    .zipWithIndex
    .flatMap(s => s._1 map(_+s._2*12))
    .filter(_<=midiRange.max)

def getDegreePitch(d: Int) = {
  degreeMap.get(d).get
}
getDegreePitch(1)
getDegreePitch(2)

def getDegreeIndex(d:Int) = {
  pitches.indexOf(getDegreePitch(d))
}

getDegreeIndex(1)
getDegreeIndex(2)

//val pitches = Seq(1,3,5,1)
//val rhythm = Rhythm(4,Seq(1,2,3,4),Seq(q,q,q,q))
//val bar = Bar(pitches, rhythm)
//PatternPlayer(bar.notes)