object Day14 extends Base(14) {

  val input = inputLines.head.toInt

  def makeRecipes(elves: IndexedSeq[Int], recipes: IndexedSeq[Int]): String =
    if (recipes.size > input + 10)
      recipes.slice(input, input + 10).mkString
    else {
      val newRecipes = recipes ++ elves.map(recipes).sum.toString.map(_.toString.toInt)
      makeRecipes(elves.map(r => (r + recipes(r) + 1) % newRecipes.size), newRecipes)
    }

  override def part1 = // 1132413111
    makeRecipes(IndexedSeq(0, 1), IndexedSeq(3, 7))

  def makeRecipes2(elves: IndexedSeq[Int], recipes: IndexedSeq[Int]): Int = {
    val x = recipes.indexOfSlice(input.toString.map(_.toString.toInt), recipes.size - 10)
    if (x > 0)
      x
    else {
      val newRecipes = recipes ++ elves.map(recipes).sum.toString.map(_.toString.toInt)
      makeRecipes2(elves.map(r => (r + recipes(r) + 1) % newRecipes.size), newRecipes)
    }
  }

  override def part2 = //
    makeRecipes2(IndexedSeq(0, 1), IndexedSeq(3, 7))
}
