object Day1 extends Base(1) {

  val nums = inputLines.map(_.toInt)

  override def part1 = // 518
    nums.sum

  override def part2 = // 72889
    findDuplicateSum(Stream.continually(nums).flatten, Set.empty, 0)

  def findDuplicateSum(stream: Stream[Int], sums: Set[Int], lastSum: Int): Int = {
    val sum = lastSum + stream.head
    if (sums(sum))
      sum
    else
      findDuplicateSum(stream.tail, sums + lastSum, sum)
  }
}
