object Day2 extends Base(2) {

  override def part1 = { // 7470
    val lengths = inputLines.map(_.groupBy(identity).map(_._2.length).toSet)
    lengths.count(_.contains(2)) * lengths.count(_.contains(3))
  }

  override def part2 = // kqzxdenujwcstybmgvyiofrrd
    inputLines.view.flatMap {
      code =>
        inputLines
          .view
          .map(_.zip(code).collect { case (c1, c2) if c1 == c2 => c1 }.mkString)
          .find(_.length == code.length - 1)
    }.head
}
