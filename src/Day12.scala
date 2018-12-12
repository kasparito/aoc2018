object Day12 extends Base(12) {

  val generations = 20
  val initial = Array.fill(generations)('.').mkString + inputLines.head.substring("initial state: ".length) + Array.fill(generations)('.').mkString

  val Note = """([.#]{5}) => ([.#])""".r
  val notes = inputLines.tail.tail
    .map { case Note(pattern, result) => pattern -> result }
    .toMap

  override def part1 = // 3276
    (0 until generations).foldLeft(initial) {
      case (state, generation) =>
        println(s"$generation: $state")
        (".." + state + "..").sliding(5).map(notes.getOrElse(_, ".")).mkString
    }.zipWithIndex.collect {
      case ('#', index) =>
        index - generations
    }.sum


}
