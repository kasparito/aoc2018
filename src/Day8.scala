object Day8 extends Base(8) {

  val numbers = inputLines.head.split(' ').map(_.toInt).toList

  case class Node(children: IndexedSeq[Node], metadata: List[Int])

  def buildNode(nums: List[Int], children: List[Node], childCount: Int, metadataCount: Int): (Node, List[Int]) =
    if (children.size == childCount) {
      val (metadata, restNums) = nums.splitAt(metadataCount)
      (Node(children.reverse.toIndexedSeq, metadata), restNums)
    } else {
      val (child, restNums) = buildNode(nums)
      buildNode(restNums, child :: children, childCount, metadataCount)
    }

  def buildNode(nums: List[Int]): (Node, List[Int]) =
    nums match {
      case childCount :: metadataCount :: restNums =>
        buildNode(restNums, Nil, childCount, metadataCount)
    }

  def sum(node: Node): Int =
    node.metadata.sum + node.children.map(sum).sum

  override def part1 = // 35852
    sum(buildNode(numbers)._1)

  def value(node: Node): Int =
    if (node.children.isEmpty)
      node.metadata.sum
    else
      node.metadata.map(_ - 1).collect {
        case index if node.children.isDefinedAt(index) =>
          value(node.children(index))
      }.sum

  override def part2 =
    value(buildNode(numbers)._1)
}
