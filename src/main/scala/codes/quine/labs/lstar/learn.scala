package codes.quine.labs.lstar

import scala.collection.BitSet

/** Executes Angluin's L* algorithm, then returns a learnt DFA. */
def learn[A](alphabet: Set[A], membership: Membership[A], equivalence: Equivalence[A]): Dfa[_, A] =
  val initialKey = if membership(Seq.empty) then BitSet(0) else BitSet.empty
  var ot = ObservationTable[A](Seq(Seq.empty), Map(initialKey -> Seq.empty))

  while true do
    ot = ot.makeClosed(alphabet, membership)
    val hypothesis = ot.toDfa(alphabet, membership)
    equivalence(hypothesis) match
      case Some(counterExample) =>
        ot = ot.update(hypothesis, counterExample, membership)
      case None => return hypothesis

  sys.error("unreachable")
