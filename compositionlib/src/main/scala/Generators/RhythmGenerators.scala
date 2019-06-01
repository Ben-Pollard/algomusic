package Generators

import models.Primitives._

object RhythmGenerators {

  //distributes beats as evenly as possible.
  //8,3 = tresillo
  //8,5 = cinquillo
  //4,3 = cumbia
  //16,5 = bossa nova
  def bjorklund(len: Int, notes: Int, hitDurations: Seq[Duration]): Rhythm = {

    val beats = List.fill(notes)(List(1.asInstanceOf[Byte]))
    val nonBeats =  List.fill(len - notes)(List(0.asInstanceOf[Byte]))
    val initList = if (nonBeats.length > beats.length) {
      beats.zip(nonBeats).map(b => b._1 ++ b._2) ++ nonBeats.takeRight(len - 2 * notes)
    } else{
      nonBeats.zip(beats).map(b => b._1 ++ b._2) ++ beats.takeRight(2 * notes - len)
    }


    def euclidean(len: Int, notes:Int, list: List[List[Byte]]): List[List[Byte]] = {
      if (len == 1) {
        list
      } else {
        val joinableNum = List(len, list.length - len).min
        val leftOverNum = list.length - 2 * joinableNum
        val head = list.take(joinableNum).zip(list.takeRight(joinableNum)).map(x => x._1 ++ x._2)
        val tail = list.slice(joinableNum, joinableNum + leftOverNum)
        euclidean(notes, len % notes, head ++ tail)
      }
    }

    val beatPattern = euclidean(notes, len % notes, initList).flatten

    val hitIndices = beatPattern.zipWithIndex.filter(_._1==1).map(_._2)
    Rhythm(len, hitIndices , hitDurations)

  }

  def bjorklund(steps: Int, hits: Int): Rhythm = {
    bjorklund(steps, hits, List.fill(hits)(q))
  }

}
