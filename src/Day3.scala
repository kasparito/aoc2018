import scala.io.Source

object Day3 extends App {

  implicit class KeyValuePairs[K, V](pairs: Iterable[(K, V)]) {
    def groupKeyValue: Map[K, Iterable[V]] =
      pairs.groupBy(_._1).mapValues(_.map(_._2)).view.force
  }

  val input = Source.fromFile("input/day3.txt").getLines().toList

  val Claim = """#(\d+) @ (\d+),(\d+): (\d+)x(\d+)""".r

  val coordinateClaims = {
    for {
      Claim(id, dx, dy, w, h) <- input
      x <- Range(dx.toInt, dx.toInt + w.toInt)
      y <- Range(dy.toInt, dy.toInt + h.toInt)
    } yield ((x, y), id.toInt)
  }.groupKeyValue

  println(coordinateClaims.values.count(_.size > 1)) // 118539

  val claimCounts = coordinateClaims
    .values
    .flatMap(claims => claims.map(_ -> claims.size))
    .groupKeyValue
    .mapValues(_.toList.distinct)

  println(claimCounts.collectFirst { case (id, List(1)) => id }.head) // 1270
}
