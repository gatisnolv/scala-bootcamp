package com.evolutiongaming.bootcamp.adt

import java.time.Instant
import com.evolutiongaming.bootcamp.adt.AlgebraicDataTypes.Hand.HoldemHand
import com.evolutiongaming.bootcamp.adt.AlgebraicDataTypes.Hand.OmahaHand

object AlgebraicDataTypes {

  // ALGEBRAIC DATA TYPES

  // Algebraic Data Types or ADTs is a commonly used way of structuring data, used in many programming
  // languages (so this is not something unique to Scala).
  //
  // While the definition may sound scientific and complex, in reality we have already been using this
  // concept in `basics` package. For example, see `sealed trait Shape` in `ClassesAndTraits`. We will now
  // look into the concept and its use cases in more detail.

  // ADTs are widely used in Scala for multiple reasons:
  // - to ensure it is hard or even impossible to represent invalid data;
  // - because pattern matching and ADTs play nicely together.

  // Two common classes of ADTs are:
  // 1. product types: case classes and tuples;
  // 2. sum types: sealed traits and abstract classes.

  // PRODUCT TYPES

  // A product type allows to combine multiple values into one. Canonical Scala examples of product types are
  // case classes and tuples. See `Basics` and `ClassesAndTraits` for their introduction.

  // A product type is called like that because one can calculate how many different values it can possibly
  // have by multiplying the number of such possibilities for the types it combines. The resulting number
  // is called the arity of the product type.

  // Question. What is the arity of the product type `(Boolean, Boolean)`?
  type DoubleBoolean = (Boolean, Boolean)

  // Question. What is the arity of the product type `Person`?
  final case class Person(name: String, surname: String, age: Int)

  // Question. `Int`, `Double`, `String`, etc. are useful types from the Scala standard library, which can
  // represent a wide range of data. In the product type `Person`, both the name and the surname are
  // represented by `String`. Is that a good idea?

  // VALUE CLASSES

  // Value classes are a mechanism in Scala to avoid allocating runtime objects, while still providing
  // additional type safety. Runtime objects are not allocated in most cases, but there are notable
  // exceptions, see the following link for more details:
  // https://docs.scala-lang.org/overviews/core/value-classes.html

  // `Age` has a single, public val parameter that is the underlying runtime representation. The type at
  // compile time is `Age`, but at runtime, the representation is `Int`. Case classes can also be used to
  // define value classes, see `Name`.
  class Age(val value: Int) extends AnyVal
  final case class Name(value: String) extends AnyVal

  // Type aliases may seem similar to value classes, but they provide no additional type safety. They can,
  // however, increase readability of the code in certain scenarios.
  final case class Surname(value: String) extends AnyVal
  type SurnameAlias = String // No additional type safety in comparison to `String`, arguably a bad example!

  // Question. Can you come up with an example, where using type aliases would make sense?

  // Exercise. Rewrite the product type `Person`, so that it uses value classes.

  // SMART CONSTRUCTORS

  // Smart constructor is a pattern, which allows creating only valid instances of a class.

  // Exercise. Create a smart constructor for `GameLevel` that only permits levels from 1 to 80.
  final case class GameLevel private (value: Int) extends AnyVal
  object GameLevel {
    def create(value: Int): Option[GameLevel] = if (value >= 1 && value <= 80) Some(GameLevel(value)) else None
  }

  // To disable creating case classes in any other way besides smart constructor, the following pattern
  // can be used. However, it is rather syntax-heavy and cannot be combined with value classes.
  sealed abstract case class Time private (hour: Int, minute: Int)
  object Time {
    def create(hour: Int, minute: Int): Either[String, Time] =
      if (hour < 0 || hour > 23)
        Left("Invalid hour value")
      else if (minute < 0 || minute > 59)
        Left("Invalid minute value")
      else Right(new Time(hour, minute) {})
  }

  // Exercise. Implement the smart constructor for `Time` that only permits values from 00:00 to 23:59 and
  // returns "Invalid hour value" or "Invalid minute value" strings in `Left` when appropriate.

  // Question. Is using `String` to represent `Left` a good idea?

  // SUM TYPES

  // A sum type is an enumerated type. To define it one needs to enumerate all its possible variants.
  // A custom boolean type `Bool` can serve as a canonical example.
  sealed trait Bool
  object Bool {
    final case object True extends Bool
    final case object False extends Bool
  }

  // Note that sealed keyword means that `Bool` can only be extended in the same file as its declaration.
  // Question. Why do you think sealed keyword is essential to define sum types?

  // A sum type is called like that because one can calculate how many different values it can possibly
  // have by adding the number of such possibilities for the types it enumerates. The resulting number
  // is called the arity of the sum type.

  // Question. What is the arity of the sum type `Bool`?

  // The power of sum and product types is unleashed when they are combined together. For example, consider a
  // case where multiple different payment methods need to be supported. (This is an illustrative example and
  // should not be considered complete.)
  final case class AccountNumber(value: String) extends AnyVal
  final case class CardNumber(value: String) extends AnyVal
  final case class ValidityDate(month: Int, year: Int)
  sealed trait PaymentMethod
  object PaymentMethod {
    final case class BankAccount(accountNumber: AccountNumber) extends PaymentMethod
    final case class CreditCard(cardNumber: CardNumber, validityDate: ValidityDate) extends PaymentMethod
    final case object Cash extends PaymentMethod
  }

  import PaymentMethod._

  final case class PaymentStatus(value: String) extends AnyVal
  trait BankAccountService {
    def processPayment(amount: BigDecimal, accountNumber: AccountNumber): PaymentStatus
  }
  trait CreditCardService {
    def processPayment(amount: BigDecimal, cardNumber: CreditCard): PaymentStatus
  }
  trait CashService {
    def processPayment(amount: BigDecimal): PaymentStatus
  }

  // Exercise. Implement `PaymentService.processPayment` using pattern matching and ADTs.
  class PaymentService(bankAccountService: BankAccountService, creditCardService: CreditCardService, cashService: CashService) {
    def processPayment(amount: BigDecimal, method: PaymentMethod): PaymentStatus = method match {
      case BankAccount(accountNumber) => bankAccountService.processPayment(amount, accountNumber)
      case creditCard: CreditCard     => creditCardService.processPayment(amount, creditCard)
      case Cash                       => cashService.processPayment(amount)
    }
  }

  // Let's compare that to `NaivePaymentService.processPayment` implementation, which does not use ADTs, but
  // provides roughly the same features as `PaymentService`.
  // Question. What are disadvantages of `NaivePaymentService`? Are there any advantages?
  trait NaivePaymentService { // Obviously a bad example!
    def processPayment(amount: BigDecimal, bankAccountNumber: Option[String], validCreditCardNumber: Option[String], isCash: Boolean): String = ???
  }

  // Exercise. Define an Algebraic Data Type `Car`, which has a manufacturer, a model, a production year,
  // and a license plate number (can contain from 3 to 8 upper case letters and numbers). Use value classes
  // and smart constructors as appropriate.

  // type Car = Nothing

  final case class Manufacturer(value: String) extends AnyVal
  final case class Model(value: String) extends AnyVal
  final case class ProductionYear(value: Int) extends AnyVal
  final case class LicensePlateNumber(value: String) extends AnyVal

  final case class Car(manufacturer: Manufacturer, model: Model, year: ProductionYear, licensePlateNumber: LicensePlateNumber)
  object Car {
    def create(manufacturer: String, model: String, year: Int, plateNumber: String): Option[Car] =
      if (year >= 1800 && year <= 2020 && plateNumber.length >= 3 && plateNumber.length <= 8 && plateNumber.matches("^[A-Z]*[0-9]*$"))
        Some(Car(Manufacturer(manufacturer), Model(model), ProductionYear(year), LicensePlateNumber(plateNumber)))
      else None
  }

  // def main(args: Array[String]): Unit = {
  // println(Car.create("Ford", "Focus", 1991, "ABBA0001"))
  // println(Car.create("Chevrolet", "Corvette", 2020, "XZ123"))
  // }

  // Homework. Define all algebraic data types, which would be needed to implement “Hold’em Hand Strength”
  // task you completed to join the bootcamp. Use your best judgement about particular data types to include
  // in the solution, you can model concepts like:
  //
  // 1. Suit
  // 2. Rank
  // 3. Card
  // 4. Hand (Texas or Omaha)
  // 5. Board
  // 6. Poker Combination (High Card, Pair, etc.)
  // 7. Test Case (Board & Hands to rank)
  // 8. Test Result (Hands ranked in a particular order for a particular Board, accounting for splits)
  //
  // Make sure the defined model protects against invalid data. Use value classes and smart constructors as
  // appropriate. Place the solution under `adt` package in your homework repository.

  // TODO
  // 5. Board
  // 6. Poker Combination (High Card, Pair, etc.)
  // 7. Test Case (Board & Hands to rank)
  // 8. Test Result (Hands ranked in a particular order for a particular Board, accounting for splits)

  //Consider changing Options into Eithers to better inform of parsing errors

  //Suit
  sealed trait Suit
  object Suit {
    final case object Spade extends Suit
    final case object Club extends Suit
    final case object Heart extends Suit
    final case object Diamond extends Suit
    def of(value: String): Option[Suit] = {
      value match {
        case "s" => Some(Spade)
        case "c" => Some(Club)
        case "h" => Some(Heart)
        case "d" => Some(Diamond)
        case _   => None
      }
    }
  }

  //Rank
  sealed trait Rank extends Comparable[Rank] {
    val value: Int
    override def compareTo(other: Rank): Int = value - other.value
  }
  object Rank {
    final case class Two(value: Int = 0) extends Rank
    final case class Three(value: Int = 1) extends Rank
    final case class Four(value: Int = 2) extends Rank
    final case class Five(value: Int = 3) extends Rank
    final case class Six(value: Int = 4) extends Rank
    final case class Seven(value: Int = 5) extends Rank
    final case class Eight(value: Int = 6) extends Rank
    final case class Nine(value: Int = 7) extends Rank
    final case class Ten(value: Int = 8) extends Rank
    final case class Jack(value: Int = 9) extends Rank
    final case class Queen(value: Int = 10) extends Rank
    final case class King(value: Int = 11) extends Rank
    final case class Ace(value: Int = 12) extends Rank
    def of(value: String): Option[Rank] = {
      value match {
        case "2" => Some(Two())
        case "3" => Some(Three())
        case "4" => Some(Four())
        case "5" => Some(Five())
        case "6" => Some(Six())
        case "7" => Some(Seven())
        case "8" => Some(Eight())
        case "9" => Some(Nine())
        case "T" => Some(Ten())
        case "J" => Some(Jack())
        case "Q" => Some(Queen())
        case "K" => Some(King())
        case "A" => Some(Ace())
        case _   => None
      }
    }
  }

  //Card
  final case class Card(suit: Suit, rank: Rank) extends Comparable[Card] {
    override def compareTo(other: Card): Int = rank.compareTo(other.rank)
  }
  object Card {
    def of(suit: String, rank: String): Option[Card] = (Suit.of(suit), Rank.of(rank)) match {
      case (Some(suit: Suit), Some(rank: Rank)) => Some(Card(suit, rank))
      case _                                    => None
    }
    def getCardsFromString(cards: String): Option[List[Card]] =
      //convert string into card list
      ???
  }

  //Hand
  sealed trait Hand {
    val cards: List[Card]
  }
  object Hand {
    final case class HoldemHand(cards: List[Card]) extends Hand
    final case class OmahaHand(cards: List[Card]) extends Hand
    def of(cards: String): Option[Hand] = cards.length match {
      case 4 | 8 =>
        Card.getCardsFromString(cards) match {
          case Some(cards) =>
            cards.length match {
              case 4 => Some(HoldemHand(cards))
              case _ => Some(OmahaHand(cards))
            }
          case _ => None
        }
      case _ => None
    }
  }

  // Board
  final case class Board(boardCards: List[Card])
  object Board {
    def of(boardCards: String): Option[Board] = {
      if (boardCards.length == 10) {
        Card.getCardsFromString(boardCards) match {
          case Some(cards) => Some(Board(cards))
          case None        => None
        }
      } else None
    }
  }

  // Poker Combination
  sealed trait PokerCombination extends Comparable[PokerCombination] {
    val value: Int
    override def compareTo(other: PokerCombination): Int = {
      val handStrengthTypeComparison = value - other.value
      if (handStrengthTypeComparison == 0) compareToEqualStrengthType(other) else handStrengthTypeComparison
    }
    def compareToEqualStrengthType(other: PokerCombination): Int
  }
  object PokerCombination {
    final case class HighCard(value: Int = 0, kickers: List[Rank]) extends PokerCombination {
      override def compareToEqualStrengthType(other: PokerCombination): Int = ???
    }
    final case class Pair(value: Int = 1, pairRank: Rank, kickers: List[Rank]) extends PokerCombination {
      override def compareToEqualStrengthType(other: PokerCombination): Int = ???
    }
    final case class TwoPairs(value: Int = 2, pairsRanks: List[Rank], kicker: Rank) extends PokerCombination {
      override def compareToEqualStrengthType(other: PokerCombination): Int = ???
    }
    final case class ThreeOfAKind(value: Int = 3, three: Rank, kickers: List[Rank]) extends PokerCombination {
      override def compareToEqualStrengthType(other: PokerCombination): Int = ???
    }
    final case class Straight(value: Int = 4, highestRank: Rank) extends PokerCombination {
      override def compareToEqualStrengthType(other: PokerCombination): Int = ???
    }
    final case class Flush(value: Int = 5, ranks: List[Rank]) extends PokerCombination {
      override def compareToEqualStrengthType(other: PokerCombination): Int = ???
    }
    final case class FullHouse(value: Int = 6, three: Rank, pair: Rank) extends PokerCombination {
      override def compareToEqualStrengthType(other: PokerCombination): Int = ???
    }
    final case class FourOfAKind(value: Int = 7, four: Rank, kicker: Rank) extends PokerCombination {
      override def compareToEqualStrengthType(other: PokerCombination): Int = ???
    }
    final case class StraightFlush(value: Int = 8, highestRank: Rank) extends PokerCombination {
      override def compareToEqualStrengthType(other: PokerCombination): Int = ???
    }
    def of(hand: Hand, board: Board): PokerCombination =
      ///determine PokerCombination
      hand match {
        case HoldemHand(cards) => ???
        case OmahaHand(cards)  => ???
      }
  }

  // Test Case
  final case class TestCase(boardCards: Board, hands: List[Hand])
  object TestCase {
    //parse a deal (one line of input as for the bootcamp task)
    def of(deal: String): Option[TestCase] = ???
  }

  // Test Result
  final case class TestResult(evaluatedHands: List[(Hand, PokerCombination)])
  object TestResult {
    //obtain list of hands ordered by ranks, accounting for draws in strength
    def of(board: Board, hands: List[Hand]): List[Set[Hand]] = ???
  }

  // Attributions and useful links:
  // https://nrinaudo.github.io/scala-best-practices/definitions/adt.html
  // https://alvinalexander.com/scala/fp-book/algebraic-data-types-adts-in-scala/
  // https://en.wikipedia.org/wiki/Algebraic_data_type
}
