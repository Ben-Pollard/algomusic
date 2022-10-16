package util

object SequenceTransformers {
  def rotate[A](seq: Seq[A], i: Int) = {
    val size = seq.size
    val (first, last) = seq.splitAt(size - (i % size))
    last ++ first
  }

  def reverseRotate[A](seq: Seq[A], i: Int) = {
    val size = seq.size
    val (first, last) = seq.splitAt(i % size)
    last ++ first
  }
}
