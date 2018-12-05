object Day5 extends Base(5) {

  val input = inputLines.head

  def polarize(c1: Char): Char =
    if (c1.isUpper) c1.toLower else c1.toUpper

  def reducedSize(s: String) =
    s.toStream.foldLeft(List.empty[Char]) {
      case (c1 :: remaining, c2) if c1 == polarize(c2) => remaining
      case (remaining, c) => c :: remaining
    }.size

  override def part1 = // 10888
    reducedSize(input)

  override def part2 = // 6952
    input
      .map(_.toLower)
      .toSet
      .map { char: Char => reducedSize(input.replaceAll(s"[$char${polarize(char)}]", "")) }
      .min
}
