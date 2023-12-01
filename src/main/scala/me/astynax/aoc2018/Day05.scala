package me.astynax.aoc2018

import scala.annotation.tailrec

object Day05 {
  private type Input = List[Char]

  def areReacting(a: Char, b: Char): Boolean =
    a.isLetter && b.isLetter && a.toLower == b.toLower && (
      (a.isLower && b.isUpper) || (a.isUpper && b.isLower)
    )

  @tailrec
  def oneReaction(chars: List[Char], acc: List[Char] = List()): Option[List[Char]] =
    chars match {
      case a :: b :: rest =>
        if (areReacting(a, b)) Some(acc.reverse ++ rest)
        else oneReaction(b :: rest, a :: acc)
      case _ => None
    }

  @tailrec
  def react(chars: List[Char]): List[Char] =
    oneReaction(chars) match {
      case Some(s) => react(s)
      case None => chars
    }

  def step1(items: Input): Int = react(items).size

  def step2(items: Input): Int =
    items.map(_.toLower).toSet
      .toSeq.map { c: Char =>
        step1(items.filter(_.toLower != c))
      }.min

  lazy val input: Input =
    Inputs.readLinesFrom("inputs/Day05.input").head.toList
}
