import music.polyrhythms.Runner.beatsPerBar
import util.Util.lowestCommonMultiple

(0 to lowestCommonMultiple(List(6,7))).map(i => {
  (i, i%7, i%6)
}).mkString("\n")