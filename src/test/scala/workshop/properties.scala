package workshop

import org.scalacheck.{Arbitrary, Gen, Prop, Properties}
import org.scalacheck.Prop.forAll
import workshop.adts._
import workshop.typeclasses.Eq
import workshop.typeclasses.Monoid
import workshop.typeclasses.Monoid.ops._

object properties extends Properties("Workshop") {

  def positionGen: Gen[Position] =
    for {
      x <- Gen.choose(0, 7)
      y <- Gen.choose(0, 7)
    } yield Position(x, y)

  // Implement a Generator for generating only pawns
  def pawnGen: Gen[Pawn] =
    for {
      position <- positionGen
    } yield Pawn(position)

  // Write a Generator that generates kings and queens
  def kingQueenGen: Gen[ChessPiece] =
    Gen.oneOf(positionGen.map(King), positionGen.map(Queen))

  // Implement a generator rooks, bishops and knights
  def rookBishopKnightGen: Gen[ChessPiece] =
    Gen.oneOf(positionGen.map(Rook),
              positionGen.map(Bishop),
              positionGen.map(Knight))

  // Implement an Arbitrary instance for ChessPiece using the generators earlier
  implicit def chessPieceArbitrary: Arbitrary[ChessPiece] = ???

  // write a property that checks that Pawns can only move forward
  // to do so we'll use the generator that generates pawns only
  property("Pawns move forward") = Prop.forAll(pawnGen) { (pawn: ChessPiece) =>
    ???
  }

  // Next let's think about the various type class laws we learned about, can you implement properties for them?
  def monoidAssociativity[M: Monoid: Arbitrary: Eq] =
    property("Associativity") = Prop.forAll { (m1: M, m2: M, m3: M) =>
      ((m1 |+| m2) |+| m3) == (m1 |+| (m2 |+| m3))
    }

  def monoidIdentity[M: Monoid: Arbitrary: Eq] =
    property("Identity") = Prop.forAll { (m: M) =>
      (m |+| Monoid[M].empty) == m
    }

  monoidAssociativity[String]
}
