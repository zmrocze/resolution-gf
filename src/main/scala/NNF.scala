
sealed abstract class NNFedGFFormula[T, A, X] {
  val tag : T
}

case class NNFLiteral[T, A, X](tag : T, negation : Boolean, atom : A) extends NNFedGFFormula[T, A, X]
case class NNFNot[T, A, X](tag : T, sub : NNFedGFFormula[T, A, X]) extends NNFedGFFormula[T, A, X]
case class NNFAnd[T, A, X](tag : T, left : NNFedGFFormula[T, A, X], right : NNFedGFFormula[T, A, X]) extends NNFedGFFormula[T, A, X]
case class NNFOr[T, A, X](tag : T, left : NNFedGFFormula[T, A, X], right : NNFedGFFormula[T, A, X]) extends NNFedGFFormula[T, A, X]
case class NNFAtom[T, A[_], X](tag : T, atom : A[X]) extends NNFedGFFormula[T, A[_], X]
case class NNFForall[T, A[_], X](tag : T, variable : X, guard : A[X], sub : NNFedGFFormula[T, A[_], X]) extends NNFedGFFormula[T, A[_], X]
case class NNFExist[T, A[_], X](tag : T, variable : X, guard : A[X], sub : NNFedGFFormula[T, A[_], X]) extends NNFedGFFormula[T, A[_], X]


// def nnf[A[_], X](phi : GFFormula[A, X]): NNFedGFFormula[Unit, A, X] = phi match {
//   case Not (Atom(a)) => NNFLiteral((), false, a)
//   case Not (Not(g)) => nnf(g)
//   case Not (And(g, h)) => NNFOr((), nnf (Not(g)), nnf (Not(h)))
//   case Not (Or(g, h)) => NNFAnd((), nnf (Not(g)), nnf (Not(h)))
//   case And(g, h) => NNFAnd((), (nnf(g)), (nnf(h)))
//   case Or(g, h) => NNFOr((), nnf(g), nnf(h))
//   case Atom(a) => NNFLiteral((), true, a)
//   case Not (Forall(variable, p, g)) => NNFExist ((), variable, p, (nnf (Not(g))))
//   case Not (Exist(variable, p, g)) => NNFForall((), variable, p, (nnf (Not(g))))
//   case Exist(variable, p, g) => NNFExist((), variable, p, nnf(g))
//   case Forall(variable, p, g) => NNFForall ((), variable, p, nnf(g))
// }

case class AtomicFormula[R, X](relation : R, varlist : List[X])
