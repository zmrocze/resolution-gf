import cats.syntax.flatMap

sealed trait SkolemedSubFormula[X]:
    

case class SkolemLiteral[X](sign : Boolean, atom : AtomicFormula[X]) extends SkolemedSubFormula[X]
case class SkolemAnd[X](left : SkolemedSubFormula[X], right : SkolemedSubFormula[X]) extends SkolemedSubFormula[X]
case class SkolemOr[X](left : SkolemedSubFormula[X], right : SkolemedSubFormula[X]) extends SkolemedSubFormula[X]

// Represents formula of form:
// Forall(variables1) guard1 -> Forall(variables2) guard2 -> subf
// where one quantifier may be empty
case class SkolemedFormula[X](
    variables1 : List[X], guard1 : AtomicFormula[X], 
    quantifier2 : Option[(List[X], AtomicFormula[X])],
    sub : SkolemedSubFormula[X]):
    
    def variables2() = this.quantifier2 match
        case None => None
        case Some((vars2, _)) => vars2
    def quard2() = this.quantifier2 match
        case None => None
        case Some((guard2, _)) => guard2


// The argument types should be more precise, returned is Forall (psi) where psi is quantifier free, 
// phi should be Forall (psi) where psi only contains existentail quantifiers
def skolem[T, X]
  (phi : StructedFormula[X]) 
  : SkolemedFormula[X] = 

    val allUsedFunctionalSymbols : Set[FunctionalSymbol] = {
        def rec(psi : StructedSubFormula[X]) : Set[FunctionalSymbol] = psi match
            case StructLiteral(sign, atom) => atom.usedFunctionSymbols
            case StructAnd(left, right) => rec(left) ++ rec(right)
            case StructOr(left, right) => rec(left) ++ rec(right)
            case StructExist(variable, guard, sub) => rec(sub) concat guard.usedFunctionSymbols

        phi match 
            case ro @ StructTopForall(variables1, guard1, quantifier2, sub) =>
                guard1.usedFunctionSymbols ++ (ro.guard2().map(_.usedFunctionSymbols).toList.flatten)
    }
    var newFunctionalSymbols = relationalSymbols . filter (x => ! (allUsedFunctionalSymbols contains x))
    
    def newFun() : RelationalSymbol = newFunctionalSymbols match
        case x #:: xs => {
            newFunctionalSymbols = xs
            x
        }
        case _ => throw new Error("List is infinite, impossible.")

    def skolemRec(bounded : Set[X], psi : StructedFormula[X]) : SkolemedSubFormula[X] = psi match
        case StructLiteral(sign, atom) => SkolemLiteral(sign, atom)
        case StructAnd(left, right) => SkolemAnd(skolemRec(bounded, left), skolemRec(bounded, right))
        case StructOr(left, right) => SkolemOr(skolemRec(bounded, left), skolemRec(bounded, right))
        case StructExist(variables, guard, sub) => skolemRec(bounded, 
            variables.foldLeft[StructedFormula[X]]
                (SkolemAnd(SkolemLiteral(true, guard), sub))
                ((phi, x) => phi.substituted(x, FuncTerm(newFun(), bounded.toList.map(VarTerm(_)))))
            )

    val a = a
    a

    // phi match
    //     case NNFLiteral(tag, sign, atom) => 
    //     case NNFAnd(tag, left, right) =>
    //     case NNFOr(tag, left, right) =>
    //     case NNFForall(tag, variables, guard, sub) =>
    //     case NNFExist(tag, variable, guard, sub) =>
  

  