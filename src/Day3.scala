object Day3 extends Base(3) {

  val Claim = """#(\d+) @ (\d+),(\d+): (\d+)x(\d+)""".r

  val coordinateClaims = {
    for {
      Claim(id, dx, dy, w, h) <- inputLines
      x <- Range(dx.toInt, dx.toInt + w.toInt)
      y <- Range(dy.toInt, dy.toInt + h.toInt)
    } yield ((x, y), id.toInt)
  }.groupKeyValue

  override def part1 = // 118539
    coordinateClaims
      .values
      .count(_.size > 1)

  override def part2 = // 1270
    coordinateClaims
      .values
      .flatMap(claims => claims.map(_ -> claims.size))
      .groupKeyValue
      .mapValues(_.toList.distinct)
      .collectFirst { case (id, List(1)) => id }
      .head
}
