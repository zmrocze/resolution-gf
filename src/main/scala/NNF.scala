

sealed trait NNFedGFFormula[A]
case class NNFLiteral[A](negation : Boolean, atom : A) extends NNFedGFFormula[A]
case class NNFNot[A](sub : NNFedGFFormula[A]) extends NNFedGFFormula[A]
case class NNFAnd[A](left : NNFedGFFormula[A], right : NNFedGFFormula[A]) extends NNFedGFFormula[A]
case class NNFOr[A](left : NNFedGFFormula[A], right : NNFedGFFormula[A]) extends NNFedGFFormula[A]
case class NNFAtom[A](atom : A) extends NNFedGFFormula[A]
case class NNFForall[A](guard : A, sub : NNFedGFFormula[A]) extends NNFedGFFormula[A]
case class NNFExist[A](guard : A, sub : NNFedGFFormula[A]) extends NNFedGFFormula[A]


def nnf[A](phi : GFFormula[A]): NNFedGFFormula[A] = phi match {
  case Not (Atom(a)) => NNFLiteral(false, a)
  case Not (Not(g)) => nnf(g)
  case Not (And(g, h)) => NNFOr(nnf (Not(g)), nnf (Not(h)))
  case Not (Or(g, h)) => NNFAnd(nnf (Not(g)), nnf (Not(h)))
  case And(g, h) => NNFAnd((nnf(g)), (nnf(h)))
  case Or(g, h) => NNFOr(nnf(g), nnf(h))
  case Atom(a) => NNFLiteral(true, a)
  case Not (Forall(p, g)) => NNFExist (p, (nnf (Not(g))))
  case Not (Exist(p, g)) => NNFForall(p, (nnf (Not(g))))
  case Exist(p, g) => NNFExist(p, nnf(g))
  case Forall(p, g) => NNFForall (p, nnf(g))
}

