import scala.collection.SortedSet

object Day7 extends Base(7) {

  val Instruction = """Step (\w) must be finished before step (\w) can begin.""".r

  val deps = inputLines.map {
    case Instruction(step1, step2) =>
      step2.head -> step1.head
  }.groupBy(_._1).mapValues(_.map(_._2).sorted).view.force

  val steps = SortedSet[Char]() ++ deps.flatMap { case (step1, step2) => step1 :: step2 }

  def nextSteps(remaining: SortedSet[Char]): List[Char] =
    remaining.find { step => deps.get(step).forall(_.forall(dep => !remaining.contains(dep))) } match {
      case Some(next) => next :: nextSteps(remaining - next)
      case None => Nil
    }

  override def part1 = // ABGKCMVWYDEHFOPQUILSTNZRJX
    nextSteps(steps).mkString

  val workerCount = 5
  val addSeconds = 60

  def finished(time: Int, workers: List[(Char, Int)], remaining: SortedSet[Char], done: Set[Char]): Int =
    workers.partition(_._2 > time) match {
      case (remainingWorkers, doneWorkers) if doneWorkers.nonEmpty =>
        finished(time, remainingWorkers, remaining, done ++ doneWorkers.map(_._1))
      case _ =>
        remaining.find { step => deps.get(step).forall(_.forall(dep => done.contains(dep))) } match {
          case Some(next) if workers.size < workerCount =>
            finished(time, (next, time + addSeconds + 1 + next - 'A') :: workers, remaining - next, done)
          case _ if remaining.nonEmpty =>
            finished(workers.map(_._2).min, workers, remaining, done)
          case None =>
            workers.map(_._2).max
        }
    }

  override def part2 = // 898
    finished(0, Nil, steps, Set.empty)
}
