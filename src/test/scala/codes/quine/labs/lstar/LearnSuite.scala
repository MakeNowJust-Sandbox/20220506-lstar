package codes.quine.labs.lstar

class LearnSuite extends munit.FunSuite {
  test("learn") {
    val words = (0 until 4)
      .scanLeft(Seq(Seq.empty[Char]))((words, _) => words.flatMap(word => Seq('a', 'b').map(word :+ _)))
      .flatten
    val membership = (word: Seq[Char]) => word.size % 3 == 0

    val dfa = learn(
      Set('a', 'b'),
      membership,
      dfa => words.find(word => dfa.run(word) != membership(word))
    )

    assertEquals(
      dfa,
      Dfa(
        Set(Seq.empty, Seq('a'), Seq('a', 'a')),
        Set('a', 'b'),
        Seq.empty,
        Set(Seq.empty),
        Map(
          (Seq.empty, 'a') -> Seq('a'),
          (Seq.empty, 'b') -> Seq('a'),
          (Seq('a'), 'a') -> Seq('a', 'a'),
          (Seq('a'), 'b') -> Seq('a', 'a'),
          (Seq('a', 'a'), 'a') -> Seq.empty,
          (Seq('a', 'a'), 'b') -> Seq.empty
        )
      )
    )
  }
}
