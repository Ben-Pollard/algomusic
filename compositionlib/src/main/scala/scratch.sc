import org.jfugue.pattern.Pattern
import org.jfugue.theory.ChordProgression

val p = new ChordProgression("|I ii iii IV V VI vii")
  .setKey("C")
    .getPattern.toString


val tempo = "T120 "
val channel = "v0 " //v9 is percussion

PatternPlayer(new Pattern(tempo + channel + p))

//val patterns = Vector(p1, p2)

