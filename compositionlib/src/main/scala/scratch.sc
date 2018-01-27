import org.jfugue.pattern.Pattern
import org.jfugue.theory.ChordProgression
import primitives.Mappings
import primitives.Primitives._

val p = new ChordProgression("|I ii iii IV V VI vii")
  .setKey("C")
    .getPattern.toString


val tempo = "T120 "
val channel = "v0 " //v9 is percussion

PatternPlayer(new Pattern(tempo + channel + p))

Seq((1,3,5)
new Bar(numBeats = 4, notes = Seq())