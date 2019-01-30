package workshop

import workshop.monoids.{IO, Monad}
import workshop.monoids.Monad.ops._
import scala.io.StdIn

object tagless {

  trait Console[F[_]] {
    def printLine(s: String): F[Unit]
    def readLine: F[String]
  }

  object Console {
    def apply[F[_]](implicit ev: Console[F]): Console[F] = ev
  }

  // Rewrite the `monoids.consoleProgram` from earlier using tagless final style
  def consoleProgram[F[_]: Monad: Console]: F[Unit] =
    for {
      _ <- Console[F].printLine("Hello, please provide your name")
      name <- Console[F].readLine
      _ <- Console[F].printLine(s"Hi ${name}")
    } yield ()

  //create a Console interpreter for IO
  implicit def consoleIO: Console[IO] = new Console[IO] {
    def printLine(s: String): IO[Unit] = IO(() => println(s))

    def readLine: IO[String] = IO(() => StdIn.readLine)
  }

  def testconsoleProgramIO = consoleProgram[IO].unsafeRun()

  type Id[A] = A

  implicit def monadId: Monad[Id] = new Monad[Id] {
    def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = f(fa)

    def unit: Id[Unit] = ()

    override def pure[A](a: A): A = a
  }

  //create a Console interpreter for Id that does nothing for printing and returns a static string for reading
  implicit def consoleId: Console[Id] = new Console[Id] {
    def printLine(s: String): Unit = println(s"Hello ${s}")

    def readLine: Id[String] = "Rafal"
  }

  def testconsoleProgramId = consoleProgram[Id]

  // Create a Tagless algebra for accesing the file system
  trait FileSystem[F[_]]

  object FileSystem {
    def apply[F[_]](implicit ev: FileSystem[F]): FileSystem[F] = ev
  }

  // Rewrite the `monoids.fileProgram` from earlier using tagless final style
  def fileProgram = ???
}
