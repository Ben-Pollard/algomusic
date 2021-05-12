package util

object Util {

  def lowestCommonMultiple(list: Seq[Int]): Int =list.foldLeft(1:Int){
    (a, b) => b * a /
      Stream.iterate((a,b)){case (x,y) => (y, x%y)}.dropWhile(_._2 != 0).head._1.abs
  }

  def possibleTimeSigs(subdivs: Int) = {
    println((2 until subdivs toList).filter(i => subdivs % i == 0).map(i => (i, subdivs/i)))
  }

}
