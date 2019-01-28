package workshop

import workshop.adts._
import workshop.model.rawText
import simulacrum.typeclass
import scala.concurrent.Future
import workshop.typeclasses.Show.ops._
import workshop.typeclasses.Eq.ops._
import workshop.typeclasses.Monoid.ops._
import workshop.typeclasses.Functor.ops._
import scala.concurrent.ExecutionContext.Implicits.global

object typeclasses {

  //Show

  @typeclass trait Show[A] {
    def show(a: A): String
  }

  implicit def showInt: Show[Int] = new Show[Int] {
    def show(a: Int): String = a.toString
  }

  implicit def showString: Show[String] = new Show[String] {
    def show(a: String): String = a
  }

  implicit def showChessPiece: Show[ChessPiece] = new Show[ChessPiece] {
    def show(a: ChessPiece): String =
      s"""${a.chessType.getClass.getCanonicalName} : ${a.position.x.toString}, ${a.position.y.toString}"""
  }

  implicit def showOption[A: Show]: Show[Option[A]] = new Show[Option[A]] {
    def show(a: Option[A]): String = a match {
      case None    => "None"
      case Some(x) => s"Some(${x})"
    }
  }

  //Eq

  @typeclass trait Eq[A] {
    def eqv(x: A, y: A): Boolean
    def ===(x: A)(y: A): Boolean = eqv(x, y)
  }

  implicit def eqInt: Eq[Int] = new Eq[Int] {
    def eqv(x: Int, y: Int): Boolean = x == y
  }

  implicit def eqString: Eq[String] = new Eq[String] {
    def eqv(x: String, y: String): Boolean = x.equals(y)
  }

  implicit def eqOption[A: Eq]: Eq[Option[A]] = new Eq[Option[A]] {
    def eqv(x: Option[A], y: Option[A]): Boolean = (x, y) match {
      case (Some(a), Some(b)) => a === b
      case (None, None)       => true
      case _                  => false
    }
  }

  implicit def eqEither[A: Eq, B: Eq]: Eq[Either[A, B]] = new Eq[Either[A, B]] {
    def eqv(x: Either[A, B], y: Either[A, B]): Boolean = (x, y) match {
      case (Left(a), Left(b))   => a === b
      case (Right(a), Right(b)) => a === b
      case _                    => false
    }
  }

  //Monoid

  @typeclass trait Monoid[A] {
    def empty: A
    def combine(x: A, y: A): A

    def |+|(x: A)(y: A): A = combine(x, y)
  }

  implicit def intMonoid: Monoid[Int] = new Monoid[Int] {
    def empty: Int = 0

    def combine(x: Int, y: Int): Int = x + y
  }

  implicit def stringMonoid: Monoid[String] = new Monoid[String] {
    def empty: String = ""

    def combine(x: String, y: String): String = x + y
  }

  implicit def timespanMonoid: Monoid[TimeSpan] = new Monoid[TimeSpan] {
    def empty: TimeSpan = TimeSpan(0, Second)

    def combine(x: TimeSpan, y: TimeSpan): TimeSpan = addTimeSpan(x, y)
  }

  implicit def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    def empty: List[A] = List.empty

    def combine(x: List[A], y: List[A]): List[A] = x ::: y

  }

  // The intMonoid further up uses `addition` as its method of combination, but that is not the only monoid for `Int`!
  // We can also define one for multiplication, though if we just define it for `Int` again the compiler won't know which to pick
  // What we can do instead is define a small wrapper that allows us to multiply
  case class Mult(value: Int)

  implicit def multMonoid: Monoid[Mult] = new Monoid[Mult] {
    def empty: Mult = Mult(1)

    def combine(x: Mult, y: Mult): Mult = Mult(x.value * y.value)
  }

  def combineAll[A: Monoid](list: List[A]): A = {
    list.foldLeft(Monoid[A].empty)((x, y) => Monoid[A].combine(x, y))
  }

  def foldMap[A, B: Monoid](list: List[A])(f: A => B): B =
    list.foldLeft(Monoid[B].empty)((x, y) => x.combine(f(y)))

  implicit def tupleMonoid[A: Monoid, B: Monoid]: Monoid[(A, B)] =
    new Monoid[(A, B)] {
      def empty: (A, B) = (Monoid[A].empty, Monoid[B].empty)

      def combine(x: (A, B), y: (A, B)): (A, B) =
        (Monoid[A].combine(x._1, y._1), Monoid[B].combine(x._2, y._2))
    }

  implicit def tuple3Monoid[A: Monoid, B: Monoid, C: Monoid]
    : Monoid[(A, B, C)] = new Monoid[(A, B, C)] {
    def empty: (A, B, C) = (Monoid[A].empty, Monoid[B].empty, Monoid[C].empty)

    def combine(x: (A, B, C), y: (A, B, C)): (A, B, C) =
      (Monoid[A].combine(x._1, y._1),
       Monoid[B].combine(x._2, y._2),
       Monoid[C].combine(x._3, y._3))
  }

  implicit def mapMonoid[A, B: Monoid]: Monoid[Map[A, B]] =
    new Monoid[Map[A, B]] {
      def empty: Map[A, B] = Map.empty

      def combine(x: Map[A, B], y: Map[A, B]): Map[A, B] = {
        val keys: Set[A] = Set.empty[A] ++ x.keySet ++ y.keySet
        keys.map { k =>
          (k, x.get(k), y.get(k)) match {
            case (key, Some(a: B), Some(b: B)) => key -> a.combine(b)
            case (key, None, None)             => key -> Monoid[B].empty
            case (key, Some(a: B), None)       => key -> a
            case (key, None, Some(b: B))       => key -> b
          }
        }.toMap
      }
    }

  implicit def futureMonoid[A: Monoid]: Monoid[Future[A]] =
    new Monoid[Future[A]] {
      def empty: Future[A] = Future.successful[A](Monoid[A].empty)

      def combine(x: Future[A], y: Future[A]): Future[A] =
        for {
          a <- x
          b <- y
        } yield a.combine(b)
    }

  //Monoid word count
  //Use foldMap with a Monoid to count the number of words, the number of characters and the number of occurences of each word
  //Tip: the Map and Tuple3 Monoid can help

  val words: List[String] = rawText.split(" ").toList

  def numberOfWords(word: String): Int = 1
  def numberOfCharacters(word: String): Int = word.length
  def numberOfOccurences(word: String, list: List[String]): Int =
    list.count(x => x == word)

  val numberWords: Int = foldMap(words)(numberOfWords)
  val numberCharacters: Int = foldMap(words)(numberOfCharacters)
  val numberOccurencies: Int =
    foldMap(words)(word => numberOfOccurences(word, words))

  //Now that you have the word count let's extend it with the ability to get the longest word of the text.
  //Tip: Define a Maximum Monoid to do so

  case class Max(word: String)

  implicit def maxMonoid: Monoid[Max] = new Monoid[Max] {
    def empty: Max = Max("")

    def combine(x: Max, y: Max): Max = {
      if (x.word.lengthCompare(y.word.length) > 0)
        x
      else
        y
    }
  }

  // foldMap is quit fast, since it iterates only once over the list and returns all those data at once
  foldMap(words)(word =>
    (Max(word), (1, numberCharacters, Map(word.toLowerCase() -> 1)))) // using mapMonoid,

  //Functor

  @typeclass trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  implicit def optionFunctor: Functor[Option] = new Functor[Option] {
    def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa match {
      case Some(a: A) => Some(f(a))
      case _          => None
    }
  }

  implicit def listFunctor: Functor[List] = new Functor[List] {
    def map[A, B](fa: List[A])(f: A => B): List[B] = fa match {
      case Nil          => Nil
      case a :: Nil     => f(a) :: Nil
      case head :: tail => f(head) :: map(tail)(f)
    }
  }

  sealed trait Tree[+A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  case class Leaf[A](value: A) extends Tree[A]

  implicit def treeFunctor: Functor[Tree] = new Functor[Tree] {
    def map[A, B](fa: Tree[A])(f: A => B): Tree[B] =
      fa match {
        case Leaf(a) => Leaf(f(a))
        case Branch(left, right) =>
          Branch(map(left)(f), map(right)(f))
      }
  }

  //Cardinality

  @typeclass trait Cardinality[A] {
    def cardinality: BigInt
  }

  implicit def cardinalityUnit: Cardinality[Unit] = new Cardinality[Unit] {
    def cardinality: BigInt = 1
  }

  implicit def cardinalityBoolean: Cardinality[Boolean] =
    new Cardinality[Boolean] {
      def cardinality: BigInt = 2
    }

  implicit def cardinalityByte: Cardinality[Byte] = new Cardinality[Byte] {
    def cardinality: BigInt = 256
  }

  implicit def cardinalityShort: Cardinality[Short] = new Cardinality[Short] {
    def cardinality: BigInt = BigInt(2 ^ 16)
  }

  implicit def cardinalityInt: Cardinality[Int] = new Cardinality[Int] {
    def cardinality: BigInt = BigInt(2 ^ 32)
  }

  implicit def cardinalityTuple[A: Cardinality, B: Cardinality]
    : Cardinality[(A, B)] = new Cardinality[(A, B)] {
    def cardinality: BigInt =
      Cardinality[A].cardinality * Cardinality[B].cardinality
  }

  implicit def cardinalityEither[A: Cardinality, B: Cardinality]
    : Cardinality[Either[A, B]] = new Cardinality[Either[A, B]] {
    def cardinality: BigInt =
      Cardinality[A].cardinality + Cardinality[B].cardinality
  }

  implicit def cardinalitySize: Cardinality[Size] = new Cardinality[Size] {
    def cardinality: BigInt = 3
  }

  implicit def cardinalityNothing: Cardinality[Nothing] =
    new Cardinality[Nothing] {
      def cardinality: BigInt = 0
    }

  implicit def cardinalityFunction[A: Cardinality, B: Cardinality]
    : Cardinality[A => B] = new Cardinality[A => B] {
    def cardinality: BigInt =
      Cardinality[B].cardinality
        .pow(Cardinality[A].cardinality.toInt) // if cardinality for a is in Int range
  }

  type +[A, B] = Either[A, B]
  type *[A, B] = Tuple2[A, B]

  Cardinality[Either[Int, (Short, Byte)]].cardinality
  Cardinality[Int + Short * Byte].cardinality
}
