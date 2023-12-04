package me.astynax.aoc2018

import scala.annotation.tailrec

object Day07 {
  private type Input = List[(Char, Char)]
  private type Graph = Map[Char, Set[Char]]

  def step1(items: Input): String = walk(
    graph = fold(items),
    acc = List(),
    jobTime = { _ => 1 },
    jobs = Map.empty[Char, Int],
    workers = 1,
    steps = 0,
  )._2.mkString

  def step2(items: Input,
            workers: Int = 5,
            jobTime: Char => Int = { c => -4 + c }
           ): Int = walk(
    graph = fold(items),
    acc = List(),
    jobTime = jobTime,
    jobs = Map.empty[Char, Int],
    workers = workers,
    steps = 0,
  )._1

  @tailrec
  def walk(graph: Graph,
           acc: List[Char],
           jobTime: Char => Int,
           jobs: Map[Char, Int],
           workers: Int,
           steps: Int): (Int, List[Char]) =
    if (graph.isEmpty & jobs.isEmpty)
      (steps - 1, acc)
    else {
      val newJobs = jobs.map { case (k, v) => k -> (v - 1) }
      val ready = newJobs
        .toSeq.filter { case (_, v) => v < 1 }
        .sortBy(_._1)
        .map(_._1)
      val (jobs_, graph_, acc_) =
        if (newJobs.isEmpty | ready.nonEmpty) {
          // store ids of finished jobs
          val acc_ = acc ++ ready
          val cleanedJobs = newJobs.removedAll(ready)
          val graph_ = graph
            .map { case (k, v) => k -> v.removedAll(ready) }
            .removedAll(ready)
          // get more jobs
          val available = graph_.toSeq
            .filter { case (k, v) => v.isEmpty & !(cleanedJobs contains k) }
            .map(_._1).sorted
            .take(workers - cleanedJobs.size)
          val jobs_ = cleanedJobs ++ available.map { k => k -> jobTime(k) }
          (jobs_, graph_, acc_)
        } else
          (newJobs, graph, acc)
      walk(graph_, acc_, jobTime, jobs_, workers, steps + 1)
    }

  def fold(links: Input): Graph =
    Agg.collect[Char, Set[Char], Char, (Char, Char)](
      Set.empty,
      identity[(Char, Char)],
      links.flatMap { case (a, b) => List(
        a -> Set.empty[Char],
        b -> Set.empty[Char]
      ) }.toMap
    )(_ + _)(links)

  def decode(line: String): (Char, Char) =
    line.charAt(36) -> line.charAt(5)

  lazy val input: Input = Inputs.readLinesFrom(s"Day07.input").map(decode)
}
