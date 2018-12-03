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
  val coordinateClaims = coordinates
    .groupBy(_._1)
    .mapValues(_.map(_._2))
    .view.force

  println(coordinateClaims.values.count(_.size > 1)) // 118539

  val idCounts = coordinateClaims.values
    .flatMap(claims => claims.map(_.id -> claims.size))
    .groupBy(_._1)
    .mapValues(_.map(_._2).toSet)
    .view.force
  println(idCounts.find(_._2 == Set(1)).head._1) // 1270
}
