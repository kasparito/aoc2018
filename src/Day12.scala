object Day12 extends Base(12) {

  val initial = inputLines.head.substring("initial state: ".length)

  val Note = """([.#]{5}) => ([.#])""".r
  val notes = inputLines.tail.tail
    .map { case Note(pattern, result) => pattern -> result }
    .toMap

  def sumPotIndexes(generations: Long) = {
    var generation = 0L
    var state = initial
    var offset = 0L

    println(s"$generation ($offset): $state")

    do {
      val firstIndex = state.indexOf('#')
      val padded = "...." + state.substring(firstIndex, state.lastIndexOf('#') + 1) + "...."

      generation += 1
      state = padded.sliding(5).map(notes.getOrElse(_, ".")).mkString
      offset = offset - 2 + firstIndex

      println(s"$generation ($offset): $state")

    } while (generation < generations)

    state.zipWithIndex.collect {
      case ('#', index) =>
        offset + index
    }.sum
  }

  private def padding(index: Int) =
    if (index > 5)
      ""
    else
      Array.fill(5 - index)('.').mkString

  override def part1 = // 3276
    sumPotIndexes(20)

  override def part2 = { //
    val state = "...###.....###....###...###.....###....###.....###.....###.....###...###.....###....###....###.....###....###....###.....###....###...###...###...###...###...###....###...###."
    val offset = 50000000000L - 76
    state.zipWithIndex.collect {
      case ('#', index) =>
        offset + index
    }.sum
  }
}
