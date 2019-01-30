package workshop

import java.io.File

import simulacrum.typeclass
import workshop.typeclasses._
import workshop.abstractions.Monoidal
import workshop.typeclasses.Monoid.ops._
import workshop.monoids.Category.ops._
import workshop.monoids.Profunctor.ops._
import workshop.monoids.Monad.ops._

import scala.util.Try
import scala.concurrent.Future
import scala.io.{Source, StdIn}

object monoids {

  // Additive Monoidal

  @typeclass trait AddMonoidal[F[_]] extends Functor[F] {
    def sum[A, B](fa: F[A], fb: F[B]): F[Either[A, B]]

    def zero[A]: F[A]

    def combineK[A](x: F[A], y: F[A]): F[A] =
      map(sum(x, y))(_.merge)
  }

  // Category

  @typeclass trait Category[F[_, _]] {
    def compose[A, B, C](fab: F[A, B], fbc: F[B, C]): F[A, C]

    def identity[A]: F[A, A]

    def >>>[A, B, C](fab: F[A, B], fbc: F[B, C]): F[A, C] = compose(fab, fbc)

    def <<<[A, B, C](fbc: F[B, C], fab: F[A, B]): F[A, C] = compose(fab, fbc)
  }

  implicit def categoryFunction: Category[Function1] = new Category[Function1] {
    def compose[A, B, C](fab: A => B, fbc: B => C): A => C = a => fbc(fab(a))

    def identity[A]: A => A = a => a
  }

  type EndoFunction[A] = A => A

  implicit def monoidEndoCategory[F[_, _]: Category, A]: Monoid[F[A, A]] =
    new Monoid[F[A, A]] {
      def empty: F[A, A] = Category[F].identity

      def combine(x: F[A, A], y: F[A, A]): F[A, A] = Category[F].compose(x, y)
    }

  def plusOne: Int => Int = _ + 1

  def times3: Int => Int = _ * 3

  def plusOneTimes3: Int => Int = plusOne |+| times3

  def plusOneTimes3ToString: Int => String = _.toString

  // Different Category instances
  case class OptionFunction[A, B](apply: A => Option[B])

  case class EffectFunction[A, B](apply: A => B)

  implicit def categoryOptionFunction: Category[OptionFunction] =
    new Category[OptionFunction] {
      def identity[A]: OptionFunction[A, A] = OptionFunction(Some(_))

      def compose[A, B, C](fab: OptionFunction[A, B],
                           fbc: OptionFunction[B, C]): OptionFunction[A, C] =
        OptionFunction { a =>
          fab.apply(a) match {
            case Some(b) => fbc.apply(b)
            case None    => None
          }
        }
    }

  implicit def categoryEffectFunction: Category[EffectFunction] =
    new Category[EffectFunction] {

      def compose[A, B, C](fab: EffectFunction[A, B],
                           fbc: EffectFunction[B, C]): EffectFunction[A, C] =
        EffectFunction(fbc.apply.compose(fab.apply))

      def identity[A]: EffectFunction[A, A] = EffectFunction(a => a)
    }

  // We can define real life synchronous programs without breaking referential transparency using EffectFunction

  trait Program {
    def program: EffectFunction[List[String], Unit]

    def main(args: Array[String]): Unit =
      program.apply(args.toList)
  }

  // Profunctor

  @typeclass trait Profunctor[F[_, _]] {
    def dimap[A, B, C, D](fac: F[B, C])(f: A => B)(g: C => D): F[A, D]

    def rmap[A, B, C](fab: F[A, B])(f: B => C): F[A, C] =
      dimap(fab)(identity[A])(f)

    def lmap[A, B, C](fbc: F[B, C])(f: A => B): F[A, C] =
      dimap(fbc)(f)(identity)
  }

  implicit def profunctorFunction: Profunctor[Function1] =
    new Profunctor[Function1] {
      def dimap[A, B, C, D](fac: B => C)(f: A => B)(g: C => D): A => D =
        f >>> fac >>> g
    }

  implicit def profunctorEffectFunction: Profunctor[EffectFunction] =
    new Profunctor[EffectFunction] {
      def dimap[A, B, C, D](fac: EffectFunction[B, C])(f: A => B)(
          g: C => D): EffectFunction[A, D] =
        EffectFunction(f >>> fac.apply >>> g)
    }

  implicit def profunctorOptionFunction: Profunctor[OptionFunction] =
    new Profunctor[OptionFunction] {
      def dimap[A, B, C, D](fac: OptionFunction[B, C])(f: A => B)(
          g: C => D): OptionFunction[A, D] = ???

    }

  // Now try to define an EffectFunction that prompts you to type your name,
  // then reads your name from stdin and outputs a greeting with your name.
  // To do so, you can use the `readLine` and `printLine` functions from `util`.

  // consoleProgram.apply()
  def consoleProgram: EffectFunction[Unit, Unit] = prompt >>> readName >>> greet

  def prompt: EffectFunction[Unit, Unit] =
    util.printLine.lmap(_ => "Hello please write your name")

  def readName: EffectFunction[Unit, String] = util.readLine

  def greet: EffectFunction[String, Unit] =
    util.printLine.lmap(name => s"Hi, ${name}")

  // We can define functions that might fail with a value

  case class FailFunction[A, B](apply: A => Either[Throwable, B])

  implicit def categoryFailFunction: Category[FailFunction] =
    new Category[FailFunction] {
      def compose[A, B, C](fab: FailFunction[A, B],
                           fbc: FailFunction[B, C]): FailFunction[A, C] =
        FailFunction(a =>
          fab.apply(a) match {
            case Right(b) => fbc.apply(b)
            case Left(t)  => Left(t)
        })

      def identity[A]: FailFunction[A, A] = FailFunction(a => Right(a))
    }

  implicit def profunctorFailFunction: Profunctor[FailFunction] = ???

  trait FailProgram {
    def program: FailFunction[List[String], Unit]

    def main(args: Array[String]): Unit =
      program.apply(args.toList) match {
        case Left(t) => throw t
        case _       => ()
      }
  }

  // Next try to define a FailFunction that reads a file name from stdin, then reads from that file and prints out its content
  // You can try using the `data/test.txt` file.

  // fileProgramm.apply(())
  def fileProgram =
    readFileName >>> loadFile >>> readFile >>> printOutFile

  def readFileName: FailFunction[Unit, String] = toFailFunction(util.readLine)

  def loadFile: FailFunction[String, File] =
    toFailFunction(EffectFunction(s => new java.io.File(s)))

  def readFile: FailFunction[File, String] = util.readFile

  def printOutFile: FailFunction[String, Unit] =
    toFailFunction(util.printLine.lmap(s => s"${s}"))

  def toFailFunction[A, B](ef: EffectFunction[A, B]): FailFunction[A, B] =
    FailFunction(a => Right(ef.apply(a)))

  // Tasks

  type Task[A] = FailFunction[Unit, A]

  def newCompose[A, B](ta: Task[A])(f: FailFunction[A, B]): Task[B] = ta >>> f

  type OptionTask[A] = OptionFunction[Unit, A]

  def optionCompose[A, B](ta: OptionTask[A])(
      f: OptionFunction[A, B]): OptionTask[B] = ta >>> f

  // Monad

  @typeclass trait Monad[F[_]] extends Monoidal[F] {
    def flatMap[A, B](fa: /* Unit => */ F[A])(f: A => F[B]): /* Unit => */ F[B]

    def flatten[A](ffa: F[F[A]]): F[A] = flatMap(ffa)(identity)

    override def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
      flatMap(fa)(a => map(fb)(b => (a, b)))

    override def map[A, B](fa: F[A])(f: A => B): F[B] =
      flatMap(fa)(a => pure(f(a)))
  }

  implicit def monadOption: Monad[Option] = new Monad[Option] {
    def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa match {
      case Some(a) => f(a)
      case _       => None
    }

    def unit: Option[Unit] = Some(())
  }

  implicit def monadTask: Monad[Task] = new Monad[Task] {
    def flatMap[A, B](fa: Task[A])(f: A => Task[B]): Task[B] =
      fa >>> FailFunction(a => f(a).apply(()))

    def unit: Task[Unit] = FailFunction(_ => Right(unit)) // identity
  }

  implicit def monadEither[E]: Monad[Either[E, ?]] = new Monad[Either[E, ?]] {
    def flatMap[A, B](fa: Either[E, A])(f: A => Either[E, B]): Either[E, B] =
      fa match {
        case Right(a) => f(a)
        case Left(t)  => Left(t)
      }

    def unit: Either[E, Unit] = Right()
  }

  def composeMonadFunctions[F[_]: Monad, A, B, C](x: A => F[B],
                                                  y: B => F[C]): A => F[C] =
    a => x(a).flatMap(y)

  // Kleisli - implement category and profucntor and generlize it so that you don't need optiofunc and failfunc ... to implement with category or profunctor

  case class Kleisli[F[_], A, B](apply: A => F[B])

  implicit def categoryKleisli[F[_]: Monad]: Category[Kleisli[F, ?, ?]] =
    new Category[Kleisli[F, ?, ?]] {
      def compose[A, B, C](fab: Kleisli[F, A, B],
                           fbc: Kleisli[F, B, C]): Kleisli[F, A, C] =
        Kleisli(composeMonadFunctions(fab.apply, fbc.apply))

      def identity[A]: Kleisli[F, A, A] = Kleisli(a => Monad[F].pure(a))
    }

  implicit def profunctorKleisli[F[_]: Monad]: Profunctor[Kleisli[F, ?, ?]] =
    new Profunctor[Kleisli[F, ?, ?]] {
      def dimap[A, B, C, D](fac: Kleisli[F, B, C])(f: A => B)(
          g: C => D): Kleisli[F, A, D] = Kleisli(a => fac.apply(f(a)).map(g))
    }

  // Now that we have Kleisli, go back and redefine OptionFunction and FailFunction as a special case of Kleisli

  type OptionFunc[A, B] = Kleisli[Option, A, B] // A => Option[B]

  def test(a: OptionFunc[Int, String]): Int => Option[String] = a.apply

  type ThrowableEither[A] = Either[Throwable, A] // it is replacement for ?
  type FailFunc[A, B] = Kleisli[ThrowableEither, A, B] // A => Either[Throwable, B]
  //type FailFunc[A, B] = Kleisli[Either[Throwable, ?], A, B] // A => Either[Throwable, B]

  def test2(a: FailFunc[Int, String]): Int => ThrowableEither[String] = a.apply

  // IO
  // for comprehension is a nested flatMapping

  case class IO[A](unsafeRun: () => A) {
    def map[B](f: A => B): IO[B] = IO(() => f(unsafeRun()))
  }

  implicit def monadIO: Monad[IO] = new Monad[IO] {
    def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] =
      IO(() => f(fa.unsafeRun()).unsafeRun())

    // f(fa(unsafeRun)) will not suspend the side effects

    def unit: IO[Unit] = IO(() => ())

    override def pure[A](a: A): IO[A] = IO(() => a)
  }

  val printHello: IO[Unit] = IO(() => println("Hello"))
  val printBye: IO[Unit] = IO(() => println("bye"))

  def testIoUnsafe =
    for {
      a <- printHello
      b <- printBye
    } yield () // you will not see the prints on the console

  def testIOunsafeRun =
    testIoUnsafe
      .unsafeRun() // this will run the function now and print them on the console

  // Run both effects one after another, but only return the result of the second
  def ignoreFirstResult[A, B](fa: IO[A], fb: IO[B]): IO[B] =
    for {
      a <- fa
      b <- fb
    } yield b

  // Run both effects one after another, but only return the result of the first
  def ignoreSecondResult[A, B](fa: IO[A], fb: IO[B]): IO[A] =
    for {
      a <- fa
      b <- fb
    } yield a

  // Reimplement fileprogram using `IO` instead
  // Tip: You can use for-comprehensions, you can try writing a version with and without using for-comprehensions

  // readFileName >>> loadFile >>> readFile >>> printOutFile

  def readFileNameIO = IO(() => "data/test.txt") // StdIn.readLine

  def readFileIO(fileName: String) = IO(() => new java.io.File(fileName))

  def readFileContentIO(file: File) =
    IO(() => Source.fromFile(file).getLines.mkString)

  def readFileContent(file: File) =
    IO(() => Source.fromFile(file).getLines.mkString)

  def printFileIO(fileContent: String) = IO(() => println(fileContent))

  def attempt[A](ioa: IO[A]): IO[Either[Throwable, A]] =
    IO(() =>
      try { Right(ioa.unsafeRun()) } catch { case t: Throwable => Left(t) })

  def readFileEither(file: File) = attempt(readFileContent(file))

  def fileProgramIO =
    for {
      fileName <- readFileNameIO
      file <- readFileIO(fileName)
      fileOrEither <- readFileEither(file)
      output <- fileOrEither match {
        case Right(a) => printFileIO(a)
        case Left(t)  => printFileIO(s"Error:${t}")
      }
    } yield output

  def testfileProgramIO = fileProgramIO.unsafeRun()

  import workshop.abstractions.Traverse.ops._
  // Use IO to print out each of the names given to this function
  // You can test this using `model.userList1`
  def printAll(names: List[String]): IO[Unit] =
    names.traverse(printFileIO).map(_ => Unit)

  def testprintAll = printAll(model.userList1).unsafeRun()

}
