
def substitute1
  [T, X]
  (phi : NNFedGFFormula[T, X], variable : X , term : Term[X]) 
  : NNFedGFFormula[T, X] = phi match
    case NNFLiteral(tag, sign, AtomicFormula(relation, arglist)) => 
        NNFLiteral(tag, sign, AtomicFormula(relation, arglist.map(substitute(_, variable, term))))
    case NNFAnd(tag, left, right) => NNFAnd(tag, substitute1(left, variable, term), substitute1(right, variable, term))
    case NNFOr(tag, left, right) => NNFOr(tag, substitute1(left, variable, term), substitute1(right, variable, term))
    case NNFForall(tag, variables, guard, sub) => 
        val (guard1 , sub1) = if variables.contains(variable) 
            then (guard, sub) 
            else (substitute2(guard, variable, term) , substitute1(sub, variable, term))
        NNFForall(tag, variables, guard1, sub1)
    case NNFExist(tag, variables, guard, sub) => 
        val (guard1 , sub1) = if variables.contains(variable) 
            then (guard, sub) 
            else (substitute2(guard, variable, term) , substitute1(sub, variable, term))
        NNFExist(tag, variables, guard1, sub1)
  