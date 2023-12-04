package me.astynax.aoc2018

object Agg {
  def collect[K, V, I, A]
  (empty: V, unpack: A => (K, I), into: Map[K, V] = Map.empty[K, V])
  (combine: (V, I) => V)
  (source: Iterable[A]): Map[K, V] =
    source.foldLeft(into) { (m, a) =>
      val (k, v) = unpack(a)
      m.updatedWith(k) { _.orElse(Some(empty)).map(combine(_, v)) }
    }

  def count[A]: Iterable[A] => Map[A, Int] =
    collect[A, Int, Int, A](0, _ -> 1)(_ + _)
}
