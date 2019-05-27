package Generators

import models.Primitives._

object RhythmGenerators {

  //distributes beats as evenly as possible.
  //8,3 = tresillo
  //8,5 = cinquillo
  //4,3 = cumbia
  //16,5 = bossa nova
  def bjorklund(steps: Int, hits: Int, hitDurations: Seq[Duration]): Rhythm = {

    val startTop = List.fill(hits)(List(1.asInstanceOf[Byte])) ++ List.fill(steps % hits)(List(0.asInstanceOf[Byte]))
    val startTail =  List.fill(hits)(List(0.asInstanceOf[Byte]))

    def Euclidean(len: Int, notes:Int, list: (List[List[Byte]], List[List[Byte]])): (List[List[Byte]], List[List[Byte]]) = {
      if (list._2.length == 1) {
        list
      } else {
        val top = list._1.zip(list._2).map(x => x._1 ++ x._2)
        val tail = list._1.takeRight(len % notes)
        Euclidean(notes, len % notes, (top,tail))
      }
    }

    val (endTop, endTail) = Euclidean(steps, hits, (startTop, startTail))
    val beatPattern = (endTop ++ endTail).flatten

    val hitIndices = beatPattern.zipWithIndex.filter(_._1==1).map(_._2)
    Rhythm(steps, hitIndices, hitDurations)

  }

  def bjorklund(steps: Int, hits: Int): Rhythm = {
    bjorklund(steps, hits, List.fill(hits)(q))
  }

}
