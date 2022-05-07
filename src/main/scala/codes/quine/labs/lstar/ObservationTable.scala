package codes.quine.labs.lstar

import scala.collection.BitSet
import scala.collection.mutable

/** Membership is a membership query function. */
type Membership[A] = Seq[A] => Boolean

/** Equivalence is a equivalence query function. */
type Equivalence[A] = Dfa[Seq[A], A] => Option[Seq[A]]

/** Key represents a key of an observation table. */
type Key = BitSet

/** Returns a key corresponding to the given word. */
def computeKey[A](word: Seq[A], tests: Seq[Seq[A]], membership: Membership[A]): Key =
  BitSet(tests.zipWithIndex.filter((test, _) => membership(word ++ test)).map(_._2): _*)

/** ObservationTable is an observation table. */
final case class ObservationTable[A](tests: Seq[Seq[A]], table: Map[Key, Seq[A]]):

  /** Returns a closed observation table. */
  def makeClosed(alphabet: Set[A], membership: Membership[A]): ObservationTable[A] =
    val keySet = mutable.Set.from(table.keysIterator)
    val newTable = mutable.Map.from(table)
    val queue = mutable.Queue.from(table.valuesIterator)
    while queue.nonEmpty do
      val prefix = queue.dequeue
      for a <- alphabet do
        val newPrefix = prefix :+ a
        val newKey = computeKey(newPrefix, tests, membership)
        if !keySet.contains(newKey) then
          keySet.add(newKey)
          newTable(newKey) = newPrefix
          queue.enqueue(newPrefix)
    ObservationTable(tests, newTable.toMap)

  /** Returns a new observation table modified for resolving the given counter example. */
  def update(
      hypothesis: Dfa[Seq[A], A],
      counterExample: Seq[A],
      membership: Membership[A]
  ): ObservationTable[A] =
    val trace = hypothesis.trace(counterExample)
    val failureIndex = trace
      .sliding(2)
      .zip(counterExample.tails.sliding(2))
      .indexWhere((words1, words2) => membership(words1(0) ++ words2(0)) != membership(words1(1) ++ words2(1)))

    val newPrefix = trace(failureIndex) :+ counterExample(failureIndex)
    val newTest = counterExample.slice(failureIndex + 1, counterExample.size)
    val newTests = if tests.contains(newTest) then tests else tests :+ newTest
    val newTable = (table.values ++ Seq(newPrefix)).map(word => computeKey(word, newTests, membership) -> word).toMap
    println((newPrefix, newTest))

    ObservationTable(tests :+ newTest, newTable)

  /** Returns a DFA corresponding to this observation table. */
  def toDfa(alphabet: Set[A], membership: Membership[A]): Dfa[Seq[A], A] =
    val stateSet = table.valuesIterator.toSet
    val initialState = Seq.empty[A]
    // Since `tests(0) == ""`, `contains(0)` means `membership(prefix + "") == true`, thus it is accept state.
    val acceptStateSet = table.filter(_._1.contains(0)).map(_._2).toSet

    val transitionTable = table
      .flatMap((_, word) => alphabet.map(a => (word, a) -> table(computeKey(word :+ a, tests, membership))))

    Dfa(stateSet, alphabet, initialState, acceptStateSet, transitionTable)
