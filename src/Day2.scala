import scala.io.Source

object Day2 extends App {

  val input = Source.fromFile("input/day2.txt").getLines().toList

  val lengths = input.map(_.groupBy(identity).map(_._2.length).toSet)
  val product = lengths.count(_.contains(2)) * lengths.count(_.contains(3))
  println(product) // 7470

  val offByOne = input.view.flatMap {
    code =>
      input
        .view
        .map(_.zip(code).collect { case (c1, c2) if c1 == c2 => c1 }.mkString)
        .find(_.length == code.length - 1)
  }.head
  println(offByOne) // kqzxdenujwcstybmgvyiofrrd
}
