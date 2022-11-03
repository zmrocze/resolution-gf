
def struct[T, A[_], X](phi : NNFedGFFormula[T, X]) : Set[NNFedGFFormula[Unit, X]] =
    val allUsedRelations : Set[RelationalSymbol] = {
        def rec(psi : NNFedGFFormula[T, X]) : Set[RelationalSymbol] = psi match
            case NNFLiteral(tag, sign, atom) => Set(atom.relation)
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
    
    def structRec(phi : NNFedGFFormula[Set[X], X]) : NNFedGFFormula[Unit, X] = structRecAux(phi, false)
    def structRecAux(phi : NNFedGFFormula[Set[X], X], firstInvocation : Boolean) : NNFedGFFormula[Unit, X] = phi match
        case NNFLiteral(tag, sign, atom) => NNFLiteral((), sign, atom)
        case NNFAnd(tag, left, right) => NNFAnd((), structRec(left), structRec(right)) 
        case NNFOr(tag, left, right) => NNFOr((), structRec(left), structRec(right))
        case NNFForall(tag, variables, guard, sub) => 
            // We rewrite only subformulas
            if firstInvocation then
                return NNFForall((), variables, guard, structRec(sub))
            else
                val freeVars = tag.toList
                val newAtom = AtomicFormula(newRel(), freeVars.map(VarTerm(_)))
                val sub1 = structRec(sub)
                val defining = NNFForall((), freeVars, newAtom, NNFForall((), variables, guard, sub1))
                res += defining
                return NNFLiteral((), true,  newAtom)
        case NNFExist(tag, variable, guard, sub) => NNFExist((), variable, guard, structRec(sub))
    
    val t = structRecAux( {
        val r = tagWithFreeVariables(phi)
        println(r)
        println()
        r
    }, true)
    res += t
    return res
