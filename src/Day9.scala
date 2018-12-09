import scala.annotation.tailrec

object Day9 extends Base(9) {

  val Pattern = """(\d+) players; last marble is worth (\d+) points.*""".r

  val (playerCount, lastMarblePoints) = inputLines.head match { case Pattern(x, y) => (x.toInt, y.toInt) }

  object Marble {
    def apply(value: Int) = {
      val marble = new Marble(value, null, null)
      marble.prev = marble
      marble.next = marble
      marble
    }
  }

  class Marble(val value: Int, var prev: Marble, var next: Marble) {

    def nextMarble(value: Int): (Marble, Int) =
      if (value % 23 == 0)
        remove(value: Int)
      else
        add(value: Int)

    def add(value: Int): (Marble, Int) = {
      val marble = new Marble(value, next, next.next)
      next.next.prev = marble
      next.next = marble
      (marble, 0)
    }

    def remove(value: Int): (Marble, Int) = {
      val marble = prev.prev.prev.prev.prev.prev.prev
      marble.next.prev = marble.prev
      marble.prev.next = marble.next
      (marble.next, value + marble.value)
    }
  }

  @tailrec
  def simulate(players: IndexedSeq[BigInt], currentPlayer: Int, currentMarble: Marble, marbleCounter: Int, lastMarble: Int): BigInt =
    if (currentMarble.value == lastMarble) {
      players.max
    } else {
      val (nextMarble, score) = currentMarble.nextMarble(marbleCounter)
      val updatedPlayers = players.updated(currentPlayer, players(currentPlayer) + score)
      simulate(
        updatedPlayers,
        (currentPlayer + 1) % players.size,
        nextMarble,
        marbleCounter + 1,
        lastMarble
      )
    }

  override def part1 = // 383475
    simulate(IndexedSeq.fill(playerCount)(0), 0, Marble(0), 1, lastMarblePoints)

  override def part2 = // 3148209772
    simulate(IndexedSeq.fill(playerCount)(0), 0, Marble(0), 1, lastMarblePoints * 100)
}
