import scala.io.Source

object Day1 extends App {

  val nums = Source.fromFile("input/day1.txt").getLines().map(_.toInt).toList

  println(nums.sum) // 518

  def findDuplicateSum(stream: Stream[Int], sums: Set[Int], lastSum: Int): Int = {
    val sum = lastSum + stream.head
    if (sums(sum))
      sum
    else
      findDuplicateSum(stream.tail, sums + lastSum, sum)
  }
  println(findDuplicateSum(Stream.continually(nums).flatten, Set.empty, 0)) // 72889
}
