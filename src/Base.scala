import scala.io.Source

abstract class Base(day: Int) {

  implicit class KeyValuePairs[K, V](pairs: Iterable[(K, V)]) {
    def groupKeyValue: Map[K, Iterable[V]] =
      pairs.groupBy(_._1).mapValues(_.map(_._2)).view.force
  }

  implicit class PrintHelper[T](x: T) {
    def log(s: String): T = {
      println(s"$s: $x")
      x
    }
  }

  val inputLines = Source.fromFile(s"input/day$day.txt").getLines().toList

  def part1: Any = "TBD"
  def part2: Any = "TBD"

  def main(args: Array[String]): Unit = {
    println(s"Part 1: $part1")
    println(s"Part 2: $part2")
  }
}
