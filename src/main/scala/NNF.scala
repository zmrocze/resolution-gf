import java.{util => ju}

sealed abstract class NNFedGFFormula[T, X] {
  val tag : T
}
 
case class NNFLiteral[T, X](tag : T, negation : Boolean, atom : AtomicFormula[X]) extends NNFedGFFormula[T, X]
case class NNFNot[T, X](tag : T, sub : NNFedGFFormula[T, X]) extends NNFedGFFormula[T, X]
case class NNFAnd[T, X](tag : T, left : NNFedGFFormula[T,  X], right : NNFedGFFormula[T, X]) extends NNFedGFFormula[T, X]
case class NNFOr[T, X](tag : T, left : NNFedGFFormula[T, X], right : NNFedGFFormula[T, X]) extends NNFedGFFormula[T, X]
case class NNFForall[T, X](tag : T, variables : List[X], guard : AtomicFormula[X], sub : NNFedGFFormula[T, X]) extends NNFedGFFormula[T, X]
case class NNFExist[T, X](tag : T, variable : List[X], guard : AtomicFormula[X], sub : NNFedGFFormula[T, X]) extends NNFedGFFormula[T, X]


def nnf[X](phi : GFFormula[X]): NNFedGFFormula[Unit, X] = phi match {
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

type RelationalSymbol = String

// we need to generate new relational symbols so lets fix R right away 
// case class AtomicFormula[R, X](relation : R, varlist : List[X])
case class AtomicFormula[X](relation : RelationalSymbol, varlist : List[X])

def relationalSymbols : LazyList[RelationalSymbol] = 
  val alpha = LazyList.from("abcdefghijklmnopqrstuvwxyz")
  def go(xs : LazyList[RelationalSymbol]) : LazyList[RelationalSymbol] =
    for (a <- alpha; x <- xs)
      yield (a.toString() + x)
  
  go(LazyList())

// type AtomicFormula1[R] = ({ type T[X] = AtomicFormula[R, X] })

def tagWithParameters
  [T, R, X]
  (phi : NNFedGFFormula[T, X]) 
  : NNFedGFFormula[Set[X], X] = phi match
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
  
  
  