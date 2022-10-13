import cats.Functor

@main def hello: Unit = 
  val phi =
    Exist(List('x'), AtomicFormula("n", List('x')),
      Forall(List('y'), AtomicFormula("a", List('x', 'y')),
        Not(Exist(List('z'), AtomicFormula("p", List('x', 'y')), 
          Forall(List('x'), AtomicFormula("a", List('x', 'z')), 
            And(Atom(AtomicFormula("b", List('z', 'z'))),
                Atom(AtomicFormula("c", List('x', 'x')))))))))
  relationalSymbols take(15)
  // print(struct(nnf(phi)))
