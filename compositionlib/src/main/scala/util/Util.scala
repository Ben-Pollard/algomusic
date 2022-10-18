package util

import models.Primitives.{MidiPitch, ScaleDegree}

object Util {

  def lowestCommonMultiple(list: Seq[Int]): Int =list.foldLeft(1:Int){
    (a, b) => b * a /
      Stream.iterate((a,b)){case (x,y) => (y, x%y)}.dropWhile(_._2 != 0).head._1.abs
  }

  def possibleTimeSigs(subdivs: Int) = {
    println((2 until subdivs toList).filter(i => subdivs % i == 0).map(i => (i, subdivs/i)))
  }

  def scaleToByte(max: Int, n: Int)= {
    127 * n / max
  }

  def scaleToByte(max: Double, n: Double)= {
    127 * n / max
  }

  def idxTuple2SubIdx(idx: Seq[(Int, Int)], subdivisions: Int) = {
    idx.map(i => i._1*subdivisions + i._2)
  }

  def subIdx2Tuple(subidx: Seq[Int], subdivisions: Int) = {
    subidx.map(s => (s/subdivisions, s%subdivisions))
  }

  def diatonicDegreeIsSameNoteLetter(a: ScaleDegree, b:ScaleDegree) = {
    (a+70)%7 == (b+70)%7
  }

  def noteIsSameNoteLetter(a: MidiPitch, b:MidiPitch) = {
    (a+120)%12 == (b+120)%12
  }

}
