package s99

import java.util.NoSuchElementException

/*
 * Example:
 * scala> last(List(1, 1, 2, 3, 5, 8))
 * res0: Int = 8
 */
object P01 {
  def main(args: Array[String]): Unit = {
    println(last(List(1, 1, 2, 3, 5, 8)))
    println(last(List()))
  }

  def last(l: List[Int]): Int = l match {
    case head :: List() => head
    case head :: tail => last(tail)
    case _ => throw new NoSuchElementException
  }
}