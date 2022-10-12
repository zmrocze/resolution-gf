import cats.Functor

@main def hello: Unit = 
  println("Hello world!")

sealed trait GFFormula[A, X]
  // override def map[A[_], B[_], X, Y](fa: GFFormula[A[_], X])(f: A[X] => B[Y]): GFFormula[B] = fa match 
  //   case (Not(sub)) => Not(lift(f)(sub))
  //   case (And(left, right)) => And(lift(f)(left), lift(f)(right))
  //   case (Or(left, right)) => Or(lift(f)(left), lift(f)(right))
  //   case Atom(atom) => Atom(f(atom))
  //   case Forall(guard, sub) => Forall(f(guard), lift(f)(sub))
  //   case Exist(guard, sub) => Exist(f(guard), lift(f)(sub))

case class Not[A[_], X](sub : GFFormula[A[_], X]) extends GFFormula[A[_], X]
case class And[A[_], X](left : GFFormula[A[_], X], right : GFFormula[A[_], X]) extends GFFormula[A[_], X]
case class Or[A[_], X](left : GFFormula[A[_], X], right : GFFormula[A[_], X]) extends GFFormula[A[_], X]
case class Atom[A[_], X](atom : A[X]) extends GFFormula[A[_], X]
case class Forall[A[_], X](variable : X, guard : A[X], sub : GFFormula[A[_], X]) extends GFFormula[A[_], X]
case class Exist[A[_], X](variable : X, guard : A[X], sub : GFFormula[A[_], X]) extends GFFormula[A[_], X]
