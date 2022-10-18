
import cats.syntax.flatMap

abstract class Pretty:
  def pretty(): String = prettyPrio(-2)

  def prettyPrio(prio : Int): String

def commaInterleaved[I <: Iterable[String]](xs : I): String = 
  Iterator.unfold(xs.iterator){ ys => 
    if ! ys.hasNext then None
    else 
      val x = ys.next
      Some(x + (if ys.hasNext then ", " else ""), ys)
  }
  .fold("")(_ + _)


sealed abstract class NNFedGFFormula[T, X] extends Pretty {
  val tag : T
  override def prettyPrio(prio : Int): String = 
    def parens(b : Boolean, str : String) = if b then "(" + str + ")" else str
    this match {
      case NNFLiteral(tag, sign, atom) => (if sign then "" else "¬") + atom.pretty()
      case NNFAnd(tag, left, right) => parens ( 6 < prio, 
        left.prettyPrio(6) + " ∧ " + right.prettyPrio(6))
      case NNFOr(tag, left, right) => parens ( 4 < prio, 
        left.prettyPrio(4) + " ∨ " + right.prettyPrio(4))
      case NNFForall(tag, variables, guard, sub) => parens ( 0 < prio,
        "∀ (" + commaInterleaved(variables.map(_.toString)) + ") " + guard.pretty() + " → " + sub.prettyPrio(2))
      case NNFExist(tag, variables, guard, sub) => parens ( 0 < prio, 
        "∃ (" + commaInterleaved(variables.map(_.toString)) + ") " + guard.pretty() + " ∧ " + sub.prettyPrio(6))
  }
}

case class NNFLiteral[T, X](tag : T, sign : Boolean, atom : AtomicFormula[X]) extends NNFedGFFormula[T, X]
// case class NNFNot[T, X](tag : T, sub : NNFedGFFormula[T, X]) extends NNFedGFFormula[T, X]
case class NNFAnd[T, X](tag : T, left : NNFedGFFormula[T,  X], right : NNFedGFFormula[T, X]) extends NNFedGFFormula[T, X]
case class NNFOr[T, X](tag : T, left : NNFedGFFormula[T, X], right : NNFedGFFormula[T, X]) extends NNFedGFFormula[T, X]
case class NNFForall[T, X](tag : T, variables : List[X], guard : AtomicFormula[X], sub : NNFedGFFormula[T, X]) extends NNFedGFFormula[T, X]
case class NNFExist[T, X](tag : T, variable : List[X], guard : AtomicFormula[X], sub : NNFedGFFormula[T, X]) extends NNFedGFFormula[T, X]

def nnf[X](phi : GFFormula[X]): NNFedGFFormula[Unit, X] = phi match {
  case Not (Atom(a)) => NNFLiteral((), false, a)
  case Not (Not(g)) => nnf(g)
  case Not (And(g, h)) => NNFOr((), nnf (Not(g)), nnf (Not(h)))
  case Not (Or(g, h)) => NNFAnd((), nnf (Not(g)), nnf (Not(h)))
  case Not (Forall(variable, p, g)) => NNFExist ((), variable, p, (nnf (Not(g))))
  case Not (Exist(variable, p, g)) => NNFForall((), variable, p, (nnf (Not(g))))
  case And(g, h) => NNFAnd((), (nnf(g)), (nnf(h)))
  case Or(g, h) => NNFOr((), nnf(g), nnf(h))
  case Atom(a) => NNFLiteral((), true, a)
  case Exist(variable, p, g) => NNFExist((), variable, p, nnf(g))
  case Forall(variable, p, g) => NNFForall ((), variable, p, nnf(g))
}

type RelationalSymbol = String

// we need to generate new relational symbols so lets fix R right away 
// case class AtomicFormula[R, X](relation : R, varlist : List[X])
case class AtomicFormula[X](relation : RelationalSymbol, varlist : List[X]) extends Pretty:
  override def prettyPrio(prio : Int) : String =
    this.relation + "(" + commaInterleaved(this.varlist.map(_.toString)) + ")"

def relationalSymbols : LazyList[RelationalSymbol] = 
  val alpha = LazyList.from("abcdefghijklmnopqrstuvwxyz")
  def go(xs : LazyList[RelationalSymbol]) : LazyList[RelationalSymbol] =
    val r = for (a <- alpha; x <- xs)
        yield (a.toString() + x)  
    r #::: go(r)
  
  go(LazyList(""))

// type AtomicFormula1[R] = ({ type T[X] = AtomicFormula[R, X] })

def tagWithFreeVariables
  [T, R, X]
  (phi : NNFedGFFormula[T, X]) 
  : NNFedGFFormula[Set[X], X] = phi match
    case NNFLiteral(tag, sign, atom) => NNFLiteral(Set.from(atom.varlist), sign, atom)
    case NNFAnd(tag, left, right) => {
      val l = tagWithFreeVariables(left)
      val r = tagWithFreeVariables(right)
      NNFAnd(l.tag concat r.tag, l, r)
    }
    case NNFOr(tag, left, right) => {
      val l = tagWithFreeVariables(left)
      val r = tagWithFreeVariables(right)
      NNFOr(l.tag concat r.tag, l, r)
    }
    case NNFForall(_, variables, guard, sub) =>
      val r = tagWithFreeVariables(sub)
      NNFForall(r.tag concat guard.varlist diff Set.from(variables), variables, guard, r)
    case NNFExist(tag, variables, guard, sub) =>
      val r = tagWithFreeVariables(sub)
      NNFExist(r.tag concat guard.varlist diff Set.from(variables), variables, guard, r)
  
  
  