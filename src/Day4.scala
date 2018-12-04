import scala.io.Source


object Day4 extends App {

  val input = Source.fromFile("input/day4.txt").getLines().toList

  val Guard = """\[(.{16})\] Guard #(\d+) begins shift""".r
  val FallsAsleep = """\[(.{16})\] falls asleep""".r
  val WakesUp = """\[(.{16})\] wakes up""".r

  val guardMinutes = input
    .sorted
    .foldLeft(List.empty[(Int, Int)]) {
      case (Nil, Guard(_, id)) =>
        List((id.toInt, -1))
      case ((_, -1) :: sleeps, Guard(_, id)) =>
        (id.toInt, -1) :: sleeps
      case ((id, -1) :: sleeps, FallsAsleep(time)) =>
        (id, minute(time)) :: sleeps
      case ((id, min) :: sleeps, WakesUp(time)) =>
        (id, -1) :: Range(min, minute(time)).map((id, _)).toList ::: sleeps
    }.groupKeyValue

  val part1 = {
    val (id, minutes) = guardMinutes.maxBy(_._2.size)
    val (minute, _) = minutes.groupBy(identity).mapValues(_.size).maxBy(_._2)
    id * minute
  }
  println(part1) // 146622

  def minute(s: String): Int = s.substring(14, 16).toInt

  implicit class KeyValuePairs[K, V](pairs: Iterable[(K, V)]) {
    def groupKeyValue: Map[K, Iterable[V]] =
      pairs.groupBy(_._1).mapValues(_.map(_._2)).view.force
  }
}
