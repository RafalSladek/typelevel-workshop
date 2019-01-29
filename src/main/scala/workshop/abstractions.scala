package workshop

import workshop.typeclasses._
import workshop.model._
import simulacrum.typeclass
import scala.concurrent.Future
import abstractions.Monoidal.ops._
import abstractions.Traverse.ops._
import scala.concurrent.ExecutionContext.Implicits.global

import typeclasses.Monoid.ops._

object abstractions {

  //Multiplicative Monoidal Functors

  @typeclass trait Monoidal[F[_]] extends Functor[F] {
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]

    def unit: F[Unit]

    def pure[A](a: A): F[A] = map(unit)(_ => a)

    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
      map(product(fa, fb)) { case (a, b) => f(a, b) }
  }

  implicit def optionMonoidal: Monoidal[Option] = new Monoidal[Option] {
    def product[A, B](fa: Option[A], fb: Option[B]): Option[(A, B)] = {
      (fa, fb) match {
        case (Some(a), Some(b)) => Some((a, b))
        case (_, _)             => None
      }
    }

    def unit: Option[Unit] = Some(())

    def map[A, B](fa: Option[A])(f: A => B): Option[B] =
      fa.map(f)
  }

  implicit def futureMonoidal: Monoidal[Future] = new Monoidal[Future] {
    def product[A, B](fa: Future[A], fb: Future[B]): Future[(A, B)] =
      for {
        a <- fa
        b <- fb
      } yield (a, b)

    def unit: Future[Unit] = Future.successful(())

    def map[A, B](fa: Future[A])(f: A => B): Future[B] = fa.map(f)
  }

  // There are two possible solutions here, can you figure out which?
  implicit def listMonoidal: Monoidal[List] = new Monoidal[List] {
    def product[A, B](fa: List[A], fb: List[B]): List[(A, B)] =
      for {
        a <- fa
        b <- fb
      } yield (a, b)

    def unit: List[Unit] = List(())

    def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
  }

  // bundle(Option(2), Option(3)) => Option[List(2,3)]
  def bundle[F[_]: Monoidal, A](x: F[A], y: F[A]): F[List[A]] = {
    x.product(y).map(c => List(c._1, c._2))
  }

  // appendM(Some(2), Some[List(1,3)]) => Some[List(1,3,2)]
  def appendM[F[_]: Monoidal, A](x: F[A], y: F[List[A]]): F[List[A]] = {
    x.product(y).map { case (a, list) => list :+ a }
  }

  // sequence(List(Some(1), Some(2))) => Some(List(1,2))
  def sequence[F[_]: Monoidal, A](list: List[F[A]]): F[List[A]] =
    list.foldRight(Monoidal[F].pure(List.empty[A])) {
      (cur: F[A], acc: F[List[A]]) =>
        appendM(cur, acc)
    }

  // traverse(List(1,2,3,4))(n => if(n > 0) Some(n) else None)
  def traverse[F[_]: Monoidal, A, B](list: List[A])(f: A => F[B]): F[List[B]] =
    list.foldRight(Monoidal[F].pure(List.empty[B])) {
      (cur: A, acc: F[List[B]]) =>
        appendM(f(cur), acc)
    }

  // ff: Option[Int => String], Option[Int] => Option[String]
  def applyFunction[F[_]: Monoidal, A, B](ff: F[A => B], fa: F[A]): F[B] =
    Monoidal[F].map2(ff, fa) { (f: A => B, a: A) =>
      f(a)
    }

  //Given two Option[Int] multiply the int values if they exist or leave them unchanged if one of them doesn't
  def combineOptions(x: Option[Int], y: Option[Int]): Option[Int] =
    x.map2(y)(_ * _)

  //Foldable

  @typeclass trait Foldable[F[_]] {
    def foldMap[A, B: Monoid](fa: F[A])(f: A => B): B

    def combineAll[A: Monoid](fa: F[A]): A = foldMap(fa)(identity)
  }

  implicit def optionFoldable: Foldable[Option] = new Foldable[Option] {
    def foldMap[A, B: Monoid](fa: Option[A])(f: A => B): B = fa match {
      case Some(a) => f(a)
      case None    => Monoid[B].empty
    }
  }

  implicit def listFoldable: Foldable[List] = new Foldable[List] {
    def foldMap[A, B: Monoid](list: List[A])(f: A => B): B =
      list.foldLeft(Monoid[B].empty)((x, y) => x.combine(f(y)))
  }

  implicit def setFoldable: Foldable[Set] = new Foldable[Set] {
    def foldMap[A, B: Monoid](fa: Set[A])(f: A => B): B =
      fa.foldLeft(Monoid[B].empty)((acc, cur) => acc combine f(cur))
  }

  // Turn this foldable into a List

  import abstractions.Foldable.ops._

  // it is generic toList implementation
  def fromFoldable[F[_]: Foldable, A](fa: F[A]): List[A] =
    fa.foldMap(a => List(a))

  // Find the first element that matches the predicate
  // Hint: YOu might need to defne a new type with a new monoid

  case class FirstOption[A](value: Option[A])

  implicit def firstOptionMonoid[A]: Monoid[FirstOption[A]] =
    new Monoid[FirstOption[A]] {
      def empty: FirstOption[A] = FirstOption(None)

      def combine(x: FirstOption[A], y: FirstOption[A]): FirstOption[A] =
        x.value match {
          case Some(_) => x
          case None    => y
        }
    }

  // find(List(1,2,3,4,5),(n:Int) => n % 2 == 0) > Option[Int] = Some(2)
  def find[F[_]: Foldable, A](fa: F[A], f: A => Boolean): Option[A] =
    fa.foldMap[FirstOption[A]](a =>
        if (f(a)) FirstOption(Some(a)) else FirstOption(None))
      .value

  //Traversable
  @typeclass trait Traverse[F[_]] {
    def traverse[G[_]: Monoidal, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

    def sequence[G[_]: Monoidal, A](fga: F[G[A]]): G[F[A]] =
      traverse(fga)(identity)
  }

  implicit def listTraversable: Traverse[List] = new Traverse[List] {
    def traverse[G[_]: Monoidal, A, B](list: List[A])(
        f: A => G[B]): G[List[B]] =
      list.foldRight(Monoidal[G].pure(List.empty[B])) {
        (cur: A, acc: G[List[B]]) =>
          appendM(f(cur), acc)
      }
  }

  implicit def optionTraversable: Traverse[Option] = new Traverse[Option] {
    def traverse[G[_]: Monoidal, A, B](fa: Option[A])(
        f: A => G[B]): G[Option[B]] = fa match {
      case None    => Monoidal[G].pure(None)
      case Some(a) => f(a).map((b: B) => Some(b))
    }
  }

  implicit def eitherTraversable[E]: Traverse[Either[E, ?]] =
    new Traverse[Either[E, ?]] {
      def traverse[G[_]: Monoidal, A, B](fa: Either[E, A])(
          f: A => G[B]): G[Either[E, B]] = fa match {
        case Right(a) => f(a).map((b: B) => Right(b))
        case Left(a)  => Monoidal[G].pure(Left(a))
      }
    }

  //Validated

  sealed trait Validated[+E, +A]
  case class Valid[+A](a: A) extends Validated[Nothing, A]
  case class Invalid[+E](e: E) extends Validated[E, Nothing]

  type ValidatedList[+E, +A] = Validated[List[E], A]

  def toEither[E, A](v: Validated[E, A]): Either[E, A] = v match {
    case Valid(a)   => Right(a)
    case Invalid(a) => Left(a)
  }

  def toValidated[E, A](e: Either[E, A]): Validated[E, A] = e match {
    case Right(e) => Valid(e)
    case Left(e)  => Invalid(e)
  }

  implicit def validatedMonoidal[E: Monoid]: Monoidal[Validated[E, ?]] =
    new Monoidal[Validated[E, ?]] {
      def product[A, B](fa: Validated[E, A],
                        fb: Validated[E, B]): Validated[E, (A, B)] =
        (fa, fb) match {
          case (Valid(a), Valid(b))       => Valid(a, b)
          case (Invalid(e), Valid(b))     => Invalid(e)
          case (Valid(a), Invalid(e))     => Invalid(e)
          case (Invalid(e1), Invalid(e2)) => Invalid(e1 combine e2)
        }

      def unit: Validated[E, Unit] = Valid(())

      def map[A, B](fa: Validated[E, A])(f: A => B): Validated[E, B] =
        fa match {
          case Valid(a)   => Valid(f(a))
          case Invalid(a) => Invalid(a)
        }
    }

  implicit def validatedTraversable[E]: Traverse[Validated[E, ?]] = ???

  // Validation exercise
  // Use `Validated` and everything you've learned so far to solve this one
  // In the `model` object you can find two lists of unvalidated strings representing users
  // Your job is to check all of them whether they are valid or not.
  // To do so, you should use the `User.validate` function.
  // Once your done, you can check the difference to Either
  def allUsers = ???

  // Next we want to write a function that takes a String representing a user
  // and return the UserReport for that user using the `User.fetchReport` function
  def reportForUser(u: String): Future[ValidatedList[String, UserReport]] = ???

  // Hard: Now get all reports for all the users
  def allReports = ???

  // Nested Monoidals

  case class Nested[F[_], G[_], A](value: F[G[A]])

  implicit def nestedMonoidal[F[_]: Monoidal, G[_]: Monoidal]
    : Monoidal[Nested[F, G, ?]] = ???

  // Try implementing `allReports` using `Nested`, it should be much easier this way
  def allReportsUsingNested: Future[ValidatedList[String, List[UserReport]]] =
    ???
  @typeclass trait ContravariantFunctor[F[_]] {
    def contramap[A, B](fa: F[A])(f: B => A): F[B]
  }

  case class Predicate[A](run: A => Boolean)

  case class StringEncoder[A](run: A => String)

  implicit def predicateContravariant: ContravariantFunctor[Predicate] = ???

  implicit def stringEncoderContravariant: ContravariantFunctor[StringEncoder] =
    ???
}
