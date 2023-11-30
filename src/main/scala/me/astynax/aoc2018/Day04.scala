package me.astynax.aoc2018

object Day04 {
  private type Input = List[Rec]

  type Range = (Int, Int)
  case class Rec(guardId: Int, rests: List[Range])

  object Decoder {
    sealed trait DecoderState

    case class Shifts(recs: List[Rec]) extends DecoderState {
      def newShift(id: Int): Shift = Shift(id, List(), this)
    }

    case class Shift(id: Int, ranges: List[Range], shifts: Shifts) extends DecoderState {
      def passTo(id: Int): Shift = Shift(id, List(), shifts.copy(
        recs = shifts.recs appended Rec(this.id, ranges)
      ))

      def fallAsleep(start: Int): Sleeping = Sleeping(start, this)
    }

    case class Sleeping(start: Int, shift: Shift) extends DecoderState {
      def wakeUp(stop: Int): Shift = shift.copy(
        ranges = shift.ranges appended (start -> stop)
      )
    }

    private val pattern =
      raw"^\[[\w-]+? (?<h>\d{2}):(?<m>\d{2})] (?<v>\w+) #?(?<a>\w+).*".r

    def parse(line: String): Option[(Int, Int, String, Option[Int])] =
      for {
        m <- pattern.findFirstMatchIn(line)
        hr <- Parsing.parseInt(m.group("h"))
        mn <- Parsing.parseInt(m.group("m"))
        vb = m.group("v")
        arg: Option[Int] <-
          if (vb == "Guard")
            Parsing.parseInt(m.group("a")).map(Some(_))
          else Some(None)
      } yield (hr, mn, vb, arg)

    def fold(steps: List[String]): List[Rec] =
      steps.foldLeft(
        Shifts(List()) : DecoderState
      ) { (acc, s) =>
        (acc, parse(s)) match {
          case (_: Shifts, Some((_, _, "Guard", Some(id)))) =>
            acc.asInstanceOf[Shifts].newShift(id)
          case (_: Shift, Some((_, _, "Guard", Some(id)))) =>
            acc.asInstanceOf[Shift].passTo(id)
          case (_: Shift, Some((_, m, "falls", _))) =>
            acc.asInstanceOf[Shift].fallAsleep(m)
          case (_: Sleeping, Some((_, m, "wakes", _))) =>
            acc.asInstanceOf[Sleeping].wakeUp(m)
          case _ => throw new IllegalArgumentException(
            s"Bad state: $acc & $s"
          )
        }
      } match {
        case s: Shift => s.passTo(-1).shifts.recs // :P
        case s => throw new IllegalArgumentException(s"Bad state: $s")
      }
  }

  def totalAndHot(ranges: List[Range]): (Int, Int) = {
    val counts = ranges.foldLeft(
      Map.empty[Int, Int]
    ) { (acc, range) =>
      (range._1 until range._2).foldLeft(acc) { (m, k) =>
        m.updatedWith(k)(_.map(_ + 1).orElse(Some(1)))
      }
    }.toList
    val total = ranges.map { case (a, b) => b - a }.sum
    val hot = if (counts.isEmpty) 0 else counts.maxBy(_._2)._1
    (total, hot)
  }

  private def mergeRecs(items: Input): List[(Int, List[Range])] =
    items.foldLeft(Map.empty[Int, List[Range]]) { (acc, i) =>
      acc.updatedWith(i.guardId) {
        rests =>
          rests
            .map(_ ++ i.rests)
            .orElse(Some(i.rests))
      }
    }.toList

  def step1(items: Input): Int = {
    val (_, id, minute) = mergeRecs(items).map {
      case (k, vs) =>
        val (total, hot) = totalAndHot(vs)
        (total, k, hot)
    }.maxBy(_._1)
    id * minute
  }

  def step2(items: Input): Int = {
    val recs = mergeRecs(items)
    val (minute, (id, _)) =
      (0 to 59).map { minute =>
        minute -> recs.map { case (id, ranges) =>
          id -> ranges.count {
            case (from, to) => minute >= from && minute < to
          }
        }.maxBy(_._2)
      }.maxBy(_._2._2)
    id * minute
  }

  lazy val input: Input =
    Decoder.fold(
      Inputs.readLinesFrom(s"inputs/Day04.input")
        .sorted
    )
}
