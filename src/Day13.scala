import scala.collection.SortedSet

object Day13 extends Base(13) {

  val board = inputLines.toIndexedSeq.map(_.toIndexedSeq)

  val initialCarts = inputLines.zipWithIndex.flatMap {
    case (row, y) => row.zipWithIndex.collect {
      case ('<', x) => (x, y, Left)
      case ('>', x) => (x, y, Right)
      case ('^', x) => (x, y, Up)
      case ('v', x) => (x, y, Down)
    }
  }.zip(Stream.from(0)).map {
    case ((x, y, direction), id) =>
      Cart(id, 0, x, y, direction)
  }

  sealed trait Direction {
    val dx: Int
    val dy: Int
    val left: Direction
    val right: Direction
    def next(x: Int, y: Int) = (x + dx, y + dy)
  }
  case object Up extends Direction {
    override val dx = 0
    override val dy = -1
    override val left = Left
    override val right = Right
  }
  case object Right extends Direction {
    override val dx = 1
    override val dy = 0
    override val left = Up
    override val right = Down
  }
  case object Down extends Direction {
    override val dx = 0
    override val dy = 1
    override val left = Right
    override val right = Left
  }
  case object Left extends Direction {
    override val dx = -1
    override val dy = 0
    override val left = Down
    override val right = Up
  }

  case class Cart(id: Int, nextTurn: Int, x: Int, y: Int, direction: Direction) {
    def next: Cart = {
      val (x2, y2) = direction.next(x, y)
      val (d2, t2) = (direction, board(y2)(x2)) match {
        case (_, '+') =>
          nextTurn match {
            case 0 => (direction.left, 1)
            case 1 => (direction, 2)
            case 2 => (direction.right, 0)
          }
        case (Left | Right, '/') => (direction.left, nextTurn)
        case (Up | Down, '/') => (direction.right, nextTurn)
        case (Left | Right, '\\') => (direction.right, nextTurn)
        case (Up | Down, '\\') => (direction.left, nextTurn)
        case _ => (direction, nextTurn)
      }
      Cart(id, t2, x2, y2, d2)
    }
  }

  def move(carts: List[Cart]): (Int, Int) = {
    val sorted = carts
      .groupBy(c => (c.y, c.x))
      .toList
      .sortBy(_._1)
    val collision = sorted.collectFirst { case ((y, x), cs) if cs.size > 1 => (x, y) }
    if (collision.isDefined) {
      collision.get
    } else {
      move(sorted.map(_._2.head.next))
    }
  }

  override def part1 = // 117,62
    move(initialCarts)

  implicit val cartOrdering = Ordering.by { cart: Cart => (cart.y, cart.x, cart.id) }

  implicit class CartSet(carts: SortedSet[Cart]) {
    def find(c: Cart) = carts.find { cart => cart.x == c.x && cart.y == c.y }
  }

  def moveCarts(carts: SortedSet[Cart]): SortedSet[Cart] =
    if (carts.isEmpty)
      SortedSet.empty
    else {
      val nextCart = carts.head.next
      val tail = carts.tail
      tail.find(nextCart) match {
        case None =>
          val movedCarts = moveCarts(tail)
          movedCarts.find(nextCart) match {
            case None => movedCarts + nextCart
            case Some(cart) => movedCarts - cart
          }
        case Some(cart) =>
          moveCarts(tail - cart)
      }
    }

  def move2(carts: SortedSet[Cart]): Cart = {
    val movedCarts = moveCarts(carts)
    if (movedCarts.size == 1) {
      movedCarts.head
    } else {
      move2(movedCarts)
    }
  }

  override def part2 = //
    move2(SortedSet[Cart]() ++ initialCarts)
}
