import scala.collection.mutable

object Day11 extends Base(11) {

  val fuelCells = 300

  val serialNumber = inputLines.head.toInt

  val powers = mutable.Map[(Int, Int, Int), Int]()

  def power(x: Int, y: Int, size: Int): Int =
    powers.getOrElseUpdate((x, y, size), size match {
      case 1 =>
        val rackId = x + 10
        hundreds((rackId * y + serialNumber) * rackId) - 5
      case n =>
        power(x, y, n - 1) +
          (0 until n).map(dx => power(x + dx, y + n - 1, 1)).sum +
          (0 until n - 1).map(dy => power(x + n - 1, y + dy, 1)).sum
    })

  def hundreds(i: Int) = {
    val s = i.toString
    val len = s.length
    s.substring(len - 3, len - 2).toInt
  }

  override def part1 = // 245,14
    {
      val size = 3
      for {
        x <- 1 to fuelCells - size + 1
        y <- 1 to fuelCells - size + 1
      } yield (x, y) -> power(x, y, size)
    }.maxBy(_._2)._1

  override def part2 = // 235,206,13
    {
      for {
        size <- 1 to fuelCells
        x <- 1 to fuelCells - size + 1
        y <- 1 to fuelCells - size + 1
      } yield (x, y, size) -> power(x, y, size)
    }.maxBy(_._2)._1
}
