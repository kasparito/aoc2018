import scala.io.Source

object Day3 extends App {

  val input = Source.fromFile("input/day3.txt").getLines().toList

  val ClaimPattern = """#(\d+) @ (\d+),(\d+): (\d+)x(\d+)""".r
  case class Claim(id: Int, dx: Int, dy: Int, w: Int, h: Int) {
    val x = Range(dx, dx + w)
    val y = Range(dy, dy + h)
  }
  val claims = input.view.map {
    case ClaimPattern(id, dx, dy, w, h) =>
      Claim(id.toInt, dx.toInt, dy.toInt, w.toInt, h.toInt)
  }.toList
  val coordinates = for {
    claim <- claims
    x <- claim.x
    y <- claim.y
  } yield ((x, y), claim)

  println(coordinates.groupBy(_._1).map(_._2.length).count(_ > 1)) // 118539
}
