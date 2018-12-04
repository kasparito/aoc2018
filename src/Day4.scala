object Day4 extends Base(4) {

  val Guard = """\[(.{16})\] Guard #(\d+) begins shift""".r
  val FallsAsleep = """\[(.{16})\] falls asleep""".r
  val WakesUp = """\[(.{16})\] wakes up""".r

  def minute(s: String): Int = s.substring(14, 16).toInt

  val guardMinutes = inputLines
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

  override def part1 = { // 146622
    val (id, minutes) = guardMinutes.maxBy(_._2.size)
    val (minute, _) = minutes.groupBy(identity).mapValues(_.size).maxBy(_._2)
    id * minute
  }

  override def part2 = { // 31848
    val ((id, minute), _) = guardMinutes
      .view
      .flatMap { case (id, minutes) => minutes.map(m => (id -> m) -> 1) }
      .groupKeyValue
      .mapValues(_.sum)
      .maxBy(_._2)
    id * minute
  }
}
