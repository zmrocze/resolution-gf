import cats.Functor

@main def hello: Unit = 
  println("Hello world!")

sealed trait GFFormula[A] extends Functor[GFFormula]:
  override def map[A, B](fa: GFFormula[A])(f: A => B): GFFormula[B] = fa match 
    case (Not(sub)) => Not(lift(f)(sub))
    case (And(left, right)) => And(lift(f)(left), lift(f)(right))
    case (Or(left, right)) => Or(lift(f)(left), lift(f)(right))
    case Atom(atom) => Atom(f(atom))
    case Forall(guard, sub) => Forall(f(guard), lift(f)(sub))
    case Exist(guard, sub) => Exist(f(guard), lift(f)(sub))

case class Not[A](sub : GFFormula[A]) extends GFFormula[A]
case class And[A](left : GFFormula[A], right : GFFormula[A]) extends GFFormula[A]
case class Or[A](left : GFFormula[A], right : GFFormula[A]) extends GFFormula[A]
case class Atom[A](atom : A) extends GFFormula[A]
case class Forall[A](guard : A, sub : GFFormula[A]) extends GFFormula[A]
case class Exist[A](guard : A, sub : GFFormula[A]) extends GFFormula[A]
