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

1 to 5 map(b => if(b % 2 ==1 ) "L" else "R")
//val pitches = Seq(1,3,5,1)
//val rhythm = Rhythm(4,Seq(1,2,3,4),Seq(q,q,q,q))
//val bar = Bar(pitches, rhythm)
//PatternPlayer(bar.notes)