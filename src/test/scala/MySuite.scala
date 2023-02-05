// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite {
  test("Phi from Nivelle") {
    val phi : GFFormula[Int] =
    Exist(List(0), AtomicFormula("n", List(VarTerm(0))),
      Forall(List(1), AtomicFormula("a", List(VarTerm(0), VarTerm(1))),
        Not(Exist(List(2), AtomicFormula("p", List(VarTerm(0), VarTerm(2))), 
          Forall(List(0), AtomicFormula("a", List(VarTerm(0), VarTerm(2))), 
            And(Atom(AtomicFormula("b", List(VarTerm(2), VarTerm(2)))),
                Atom(AtomicFormula("c", List(VarTerm(0), VarTerm(0))))))))))
    assertEquals(SAT(phi), true)
  }
  test("example test that doesnt") {
    val phi : GFFormula[Int] =
    Exist(List(0), AtomicFormula("n", List(VarTerm(0))),
      Exist(List(1), AtomicFormula("a", List(VarTerm(0), VarTerm(1))),
        Or(Not(Atom(AtomicFormula("n", List(VarTerm(0))))), Not(Atom(AtomicFormula("a", List(VarTerm(0), VarTerm(1))))))))
    assertEquals(SAT(phi), false)
  }
}
