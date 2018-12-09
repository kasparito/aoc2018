object Day6 extends Base(6) {

  val PointPattern = """(\d+), (\d+)""".r

  case class Point(id: Int, x: Int, y: Int) {
    def distance(x: Int, y: Int) =
      math.abs(this.x - x) + math.abs(this.y - y)
  }

  val points = inputLines.zipWithIndex.map {
    case (PointPattern(x, y), id) =>
      Point(id, x.toInt, y.toInt)
  }

  val (rangeX, rangeY) = {
    val xs = points.map(_.x)
    val ys = points.map(_.y)
    (xs.min to xs.max, ys.min to ys.max)
  }

  def nearestPoint(x: Int, y: Int) =
    points
      .map(p => p.distance(x, y) -> p)
      .groupBy(_._1)
      .toSeq
      .sortBy(_._1)
      .headOption
      .collect { case (_, List((_, point))) => point }

  val nearestPoints = for {
    x <- rangeX
    y <- rangeY
    p <- nearestPoint(x, y)
  } yield p

  override def part1 = // 3238
    nearestPoints
      .groupBy(identity)
      .map(_._2.size)
      .max

  def distanceSum(x: Int, y: Int) =
    points.map(p => p.distance(x, y)).sum

  val distanceSums = for {
    x <- rangeX
    y <- rangeY
  } yield distanceSum(x, y)

  override def part2 = // 45046
    distanceSums.count(_ < 10000)
}
