import midi.PatternPlayer
import midi.PatternPlayer.getReceiverAndPlayer
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

val pattern = "T100 Ra0 C5q Rh E5q Rh G5q Rh C6q Rh"
val (receiver, player) = getReceiverAndPlayer()
receiver.sendSequence(player.getSequence(pattern))