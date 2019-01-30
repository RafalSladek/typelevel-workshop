package workshop

import scala.util
import scala.util.Either.RightProjection

object adts {

  // Design a data type for coffee sizes, should have small medium and large
  sealed trait Size
  case object Small extends Size
  case object Medium extends Size
  case object Large extends Size

  // Model a data type for a contact that can either be an email or a phone number
  sealed trait Contact
  case class Email(value: String) extends Contact
  case class PhoneNumber(value: String) extends Contact

  // Design a data type for a chess piece and its position on the chess board
  // Write a function using pattern matching that takes a square and returns whether it can move there or not
  sealed trait ChessPiece {
    def canMove(target: Position): Boolean

    val current: Position
  }

  case class King(current: Position) extends ChessPiece {
    override def canMove(target: Position): Boolean =
      current.getDiff(target) match {
        case (a, b) if a == b && a == 0 || a == 1 =>
          true // not moving or diagonal
        case (1, 0) => true // back or forth
        case (0, 1) => true // left or right
        case _      => false
      }
  }

  case class Queen(current: Position) extends ChessPiece {
    override def canMove(target: Position): Boolean =
      current.getDiff(target) match {
        case (_, 0)           => true
        case (0, _)           => true
        case (a, b) if a == b => true // not moving or diagonal
        case _                => false
      }
  }
  case class Rook(current: Position) extends ChessPiece {
    override def canMove(target: Position): Boolean =
      current.getDiff(target) match {
        case (0, 0) => true
        case (_, 0) => true
        case (0, _) => true
        case _      => false
      }
  } // wieza

  case class Bishop(current: Position) extends ChessPiece {
    override def canMove(target: Position): Boolean =
      current.getDiff(target) match {
        case (a, b) if a == b => true
        case _                => false
      }
  } // goniec

  case class Knight(current: Position) extends ChessPiece {
    override def canMove(target: Position): Boolean =
      current.getDiff(target) match {
        case (0, 0) => true
        case (2, 1) => true
        case (1, 2) => true
        case _      => false
      }
  } // skoczek

  case class Pawn(current: Position) extends ChessPiece {
    override def canMove(target: Position): Boolean =
      current.getDiff(target) match {
        case (0, 0) => true
        case (0, 1) => true
        case _      => false
      }
  } // pionek

  case class Position(x: Int, y: Int) {
    def getDiff(target: Position): (Int, Int) =
      (Math.abs(target.x - x), Math.abs(target.y - y))
  }

  sealed trait TimeUnit
  case object Second extends TimeUnit
  case object Minute extends TimeUnit
  case object Hour extends TimeUnit
  case object Day extends TimeUnit
  case object Month extends TimeUnit

  type Duration = Int

  // Model a data type that stores time spans
  case class TimeSpan(duration: Duration, timeUnit: TimeUnit)

  def convertToBasicUnit(timeSpan: TimeSpan): TimeSpan =
    convertToBasicUnit(timeSpan.duration, timeSpan.timeUnit)

  def convertToBasicUnit(duration: Duration, timeUnit: TimeUnit): TimeSpan = {
    timeUnit match {
      case Second => TimeSpan(duration, Second)
      case Minute => convertToBasicUnit(duration * 60, Second)
      case Hour   => convertToBasicUnit(duration * 60, Minute)
      case Day    => convertToBasicUnit(duration * 24, Hour)
      case Month  => convertToBasicUnit(duration * 30, Day)
    }
  }

  // Write a function that adds two TimeSpan values together

  def addTimeSpan(a: TimeSpan, b: TimeSpan): TimeSpan =
    TimeSpan(convertToBasicUnit(a).duration + convertToBasicUnit(b).duration,
             Second)

  // List all values of the type `Unit`
  def allValuesUnit: Set[Unit] = Set(())

  // List all values of the type `Nothing`
  def allValuesNothing: Set[Nothing] = Set.empty

  // List all values of the type `Boolean`
  def allValuesBoolean: Set[Boolean] = Set(false, true)

  // List all values of the type `Size`
  def allValuesSize: Set[Size] = Set(Small, Medium, Large)

  // List all values of the type `(Size, Boolean)`
  def allValuesTuple: Set[(Size, Boolean)] = Set(
    (Small, true),
    (Small, false),
    (Medium, false),
    (Medium, false),
    (Large, false),
    (Large, false)
  )

  // List all values of the type `Either[Size, Boolean]`
  def allValuesEither: Set[Either[Size, Boolean]] = Set(
    Left(Small),
    Left(Medium),
    Left(Large),
    Right(true),
    Right(false)
  )

  // List all values of the type `(Size, Unit)`
  def allValuesTupleUnit: Set[(Size, Unit)] = Set(
    (Small, ()),
    (Medium, ()),
    (Large, ()),
  )

  // List all values of the type `Either[Boolean, Nothing]`
  def allValuesEitherNothing: Set[Either[Boolean, Nothing]] = Set(
    Left(true),
    Left(false),
  )
}
