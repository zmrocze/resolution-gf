
sealed abstract class NNFedGFFormula[T, A[_], X] {
  val tag : T
}
 
case class NNFLiteral[T, A[_], X](tag : T, negation : Boolean, atom : A[X]) extends NNFedGFFormula[T, A, X]
case class NNFNot[T, A[_], X](tag : T, sub : NNFedGFFormula[T, A, X]) extends NNFedGFFormula[T, A, X]
case class NNFAnd[T, A[_], X](tag : T, left : NNFedGFFormula[T, A, X], right : NNFedGFFormula[T, A, X]) extends NNFedGFFormula[T, A, X]
case class NNFOr[T, A[_], X](tag : T, left : NNFedGFFormula[T, A, X], right : NNFedGFFormula[T, A, X]) extends NNFedGFFormula[T, A, X]
case class NNFForall[T, A[_], X](tag : T, variables : List[X], guard : A[X], sub : NNFedGFFormula[T, A, X]) extends NNFedGFFormula[T, A, X]
case class NNFExist[T, A[_], X](tag : T, variable : List[X], guard : A[X], sub : NNFedGFFormula[T, A, X]) extends NNFedGFFormula[T, A, X]


def nnf[A[_], X](phi : GFFormula[A, X]): NNFedGFFormula[Unit, A, X] = phi match {
  case Not (Atom(a)) => NNFLiteral((), false, a)
  case Not (Not(g)) => nnf(g)
  case Not (And(g, h)) => NNFOr((), nnf (Not(g)), nnf (Not(h)))
  case Not (Or(g, h)) => NNFAnd((), nnf (Not(g)), nnf (Not(h)))
  case And(g, h) => NNFAnd((), (nnf(g)), (nnf(h)))
  case Or(g, h) => NNFOr((), nnf(g), nnf(h))
  case Atom(a) => NNFLiteral((), true, a)
  case Not (Forall(variable, p, g)) => NNFExist ((), variable, p, (nnf (Not(g))))
  case Not (Exist(variable, p, g)) => NNFForall((), variable, p, (nnf (Not(g))))
  case Exist(variable, p, g) => NNFExist((), variable, p, nnf(g))
  case Forall(variable, p, g) => NNFForall ((), variable, p, nnf(g))
}

case class AtomicFormula[R, X](relation : R, varlist : List[X])

type AtomicFormula1[R] = ({ type T[X] = AtomicFormula[R, X] })

def tagWithParameters
  [T, R, X]
  (phi : NNFedGFFormula[T, ({ type T[X] = AtomicFormula[R, X] })#T, X]) 
  : NNFedGFFormula[Set[X], ({ type T[X] = AtomicFormula[R, X] })#T, X] = phi match
    case NNFLiteral(tag, negation, atom) => NNFLiteral(Set.from(atom.varlist), negation, atom)
    case NNFNot(tag, sub) => {
      val r = tagWithParameters(sub)
      NNFNot(r.tag, r)
    }
    case NNFAnd(tag, left, right) => {
      val l = tagWithParameters(left)
      val r = tagWithParameters(right)
      NNFAnd(l.tag concat r.tag, l, r)
    }
    case NNFOr(tag, left, right) => {
      val l = tagWithParameters(left)
      val r = tagWithParameters(right)
      NNFOr(l.tag concat r.tag, l, r)
    }
    case NNFForall(_, variables, guard, sub) =>
      val r = tagWithParameters(sub)
      NNFForall(r.tag diff Set.from(variables), variables, guard, r)
    case NNFExist(tag, variables, guard, sub) =>
      val r = tagWithParameters(sub)
      NNFExist(r.tag diff Set.from(variables), variables, guard, r)
  
  
  