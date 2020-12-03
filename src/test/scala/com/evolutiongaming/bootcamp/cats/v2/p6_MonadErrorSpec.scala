package com.evolutiongaming.bootcamp.cats.v2
import cats.effect.IO
import cats.syntax.either._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import p6_MonadError._
import p6_MonadErrorSpec._

import scala.util.Try

class p6_MonadErrorSpec extends AnyFlatSpec with Matchers {
  "MonadError retrier" should "works with Either" in {
    Retrier.retry(succeedsAfterNCallsEither(3), 1) shouldBe Left("Bad")
    Retrier.retry(succeedsAfterNCallsEither(3), 2) shouldBe Left("Bad")
    Retrier.retry(succeedsAfterNCallsEither(3), 3) shouldBe Right("Good")
  }

  "MonadError retrier" should "works with Option" in {
    Retrier.retry(succeedsAfterNCallsOption(3), 1) shouldBe None
    Retrier.retry(succeedsAfterNCallsOption(3), 2) shouldBe None
    Retrier.retry(succeedsAfterNCallsOption(3), 3) shouldBe Some("Good")
  }

  "MonadError retrier" should "works with IO" in {
    runSafe(Retrier.retry(succeedsAfterNCallsIO(3), 1)) shouldBe Left("Bad")
    runSafe(Retrier.retry(succeedsAfterNCallsIO(3), 2)) shouldBe Left("Bad")
    runSafe(Retrier.retry(succeedsAfterNCallsIO(3), 3)) shouldBe Right("Good")
  }

  "Traverse for Option" should "work fine" in {
    val inputGood = List(Some(1), Some(2), Some(3))
    val inputBad = List(Some(1), None, None)

    p7_Traverse.optionTraverse(inputGood) == Some(List(1, 2, 3))
    p7_Traverse.optionTraverse(inputBad) == None
  }
}

object p6_MonadErrorSpec {
  def runSafe[A](io: IO[A]): Either[String, A] = Try(io.unsafeRunSync()).toEither.leftMap(_.getMessage)

}
