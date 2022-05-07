package codes.quine.labs.lstar

import scala.collection.mutable

/** Dfa represents a deterministic finite-state automaton. */
final case class Dfa[Q, A](
    stateSet: Set[Q],
    alphabet: Set[A],
    initialState: Q,
    acceptStateSet: Set[Q],
    transitionTable: Map[(Q, A), Q]
):
  /** Returns the matching result against the given word. */
  def run(word: Seq[A]): Boolean =
    var q = initialState
    for a <- word do q = transitionTable((q, a))
    acceptStateSet.contains(q)

  /** Returns the trace sequence against the given word. */
  def trace(word: Seq[A]): Seq[Q] =
    val seq = Seq.newBuilder[Q]
    var q = initialState
    seq.addOne(q)
    for a <- word do
      q = transitionTable((q, a))
      seq.addOne(q)
    seq.result()

  /** Returns simplified version of this DFA.
    *
    * It renames state names to integer values.
    */
  def simplified: Dfa[Int, A] =
    val map = stateSet.zipWithIndex.toMap
    Dfa(
      map.values.toSet,
      alphabet,
      map(initialState),
      acceptStateSet.map(map),
      transitionTable.map { case (q1, a) -> q2 => (map(q1), a) -> map(q2) }
    )

  /** Returns the Graphviz representation of this DFA. */
  def toGraphviz: String =
    val sb = new mutable.StringBuilder

    sb.append("digraph {\n")
    sb.append(s"  \"\" [shape=point];\n")
    sb.append(s"  \"\" -> \"$initialState\";\n")
    for (q <- stateSet)
      sb.append(s"  \"$q\" [shape=${if (acceptStateSet.contains(q)) "double" else ""}circle];\n")
    for (((q0, a), q1) <- transitionTable)
      sb.append(s"  \"$q0\" -> \"$q1\" [label=\"$a\"];\n")
    sb.append("}")

    sb.result()
