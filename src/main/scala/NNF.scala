
sealed trait NNFedGFFormula[T, A] {
  val tag : T
}

case class NNFLiteral[T, A](tag : T, negation : Boolean, atom : A) extends NNFedGFFormula[T, A]
case class NNFNot[T, A](tag : T, sub : NNFedGFFormula[T, A]) extends NNFedGFFormula[T, A]
case class NNFAnd[T, A](tag : T, left : NNFedGFFormula[T, A], right : NNFedGFFormula[T, A]) extends NNFedGFFormula[T, A]
case class NNFOr[T, A](tag : T, left : NNFedGFFormula[T, A], right : NNFedGFFormula[T, A]) extends NNFedGFFormula[T, A]
case class NNFAtom[T, A](tag : T, atom : A) extends NNFedGFFormula[T, A]
case class NNFForall[T, A](tag : T, guard : A, sub : NNFedGFFormula[T, A]) extends NNFedGFFormula[T, A]
case class NNFExist[T, A](tag : T, guard : A, sub : NNFedGFFormula[T, A]) extends NNFedGFFormula[T, A]


def nnf[A](phi : GFFormula[A]): NNFedGFFormula[Unit, A] = phi match {
  case Not (Atom(a)) => NNFLiteral((), false, a)
  case Not (Not(g)) => nnf(g)
  case Not (And(g, h)) => NNFOr((), nnf (Not(g)), nnf (Not(h)))
  case Not (Or(g, h)) => NNFAnd((), nnf (Not(g)), nnf (Not(h)))
  case And(g, h) => NNFAnd((), (nnf(g)), (nnf(h)))
  case Or(g, h) => NNFOr((), nnf(g), nnf(h))
  case Atom(a) => NNFLiteral((), true, a)
  case Not (Forall(p, g)) => NNFExist ((), p, (nnf (Not(g))))
  case Not (Exist(p, g)) => NNFForall((), p, (nnf (Not(g))))
  case Exist(p, g) => NNFExist((), p, nnf(g))
  case Forall(p, g) => NNFForall ((), p, nnf(g))
}

