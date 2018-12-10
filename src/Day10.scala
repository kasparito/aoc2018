import scala.annotation.tailrec

object Day10 extends Base(10) {

  case class Point(id: Int, dx: Int, dy: Int)

  val PointPattern = """position=<([^,]+),([^,]+)> velocity=<([^,]+),([^,]+)>""".r

  val points = inputLines.zipWithIndex.map {
    case (PointPattern(x, y, dx, dy), id) =>
      (x.trim.toInt, y.trim.toInt, Point(id, dx.trim.toInt, dy.trim.toInt))
  }

  def move(points: List[(Int, Int, Point)]): List[(Int, Int, Point)] =
    points.map { case (x, y, p@Point(_, dx, dy)) => (x + dx, y + dy, p) }

  def dimensions(newPoints: List[(Int, Int, Point)]): (Range, Range) = {
    val xs = newPoints.map(_._1)
    val ys = newPoints.map(_._2)
    (Range(xs.min, xs.max), Range(ys.min, ys.max))
  }

  @tailrec
  def findMin(min: Int, points: List[(Int, Int, Point)], second: Int): (List[(Int, Int, Point)], Int) = {
    val newPoints = move(points)
    val newMin = dimensions(newPoints)._2.size
    if (min < newMin)
      (points, second)
    else
      findMin(newMin, newPoints, second + 1)
  }

  def printText(points: List[(Int, Int, Point)]) = {
    val xr = dimensions(points)._1
    points
      .groupBy(_._2)
      .toSeq
      .sortBy(_._1)
      .map(_._2.map(_._1).toSet)
      .map { xs =>xr.map(x => if (xs(x)) 'X' else ' ').mkString }
      .mkString("\n", "\n", "\n")
  }

  val (text, seconds) = findMin(dimensions(points)._2.size, points, 0)

  override def part1 =
    printText(text)

  override def part2 =
    seconds
}
