
def struct[T, A[_], X](phi : NNFedGFFormula[T, X]) : Any =
    val allUsedRelations : Set[RelationalSymbol] = {
        def rec(psi : NNFedGFFormula[T, X]) : Set[RelationalSymbol] = psi match
            case NNFLiteral(tag, negation, atom) => Set(atom.relation)
            case NNFNot(tag, sub) => rec(sub)
            case NNFAnd(tag, left, right) => rec(left) ++ rec(right)
            case NNFOr(tag, left, right) => rec(left) ++ rec(right)
            case NNFForall(tag, variables, guard, sub) => rec(sub) + guard.relation
            case NNFExist(tag, variable, guard, sub) => rec(sub) + guard.relation
        rec(phi)
    }
    var remainingSymbols = relationalSymbols . filter (x => ! (allUsedRelations contains x))
    def newRel() : RelationalSymbol = remainingSymbols match
        case x #:: xs => {
            remainingSymbols = xs
            x
        }
        case _ => throw new Error("List is infinite, impossible.")

    var res : Set[NNFedGFFormula[Unit, X]] = Set()

    def structRec(phi : NNFedGFFormula[Set[X], X]) : NNFedGFFormula[Unit, X] = phi match
        case NNFLiteral(tag, negation, atom) => NNFLiteral((), negation, atom)
        case NNFNot(tag, sub) => NNFNot((), structRec(sub))
        case NNFAnd(tag, left, right) => NNFAnd((), structRec(left), structRec(right)) 
        case NNFOr(tag, left, right) => NNFOr((), structRec(left), structRec(right))
        case NNFForall(tag, variables, guard, sub) => 
            val freeVars = tag.toList
            val newAtom = AtomicFormula(newRel(), freeVars)
            val sub1 = structRec(sub)
            val defining = NNFForall((), freeVars, newAtom, NNFForall((), variables, guard, sub1))
            res += defining
            return NNFLiteral((), true,  newAtom)
        case NNFExist(tag, variable, guard, sub) => NNFExist((), variable, guard, structRec(sub))
    
    structRec(tagWithParameters(phi))
    return res