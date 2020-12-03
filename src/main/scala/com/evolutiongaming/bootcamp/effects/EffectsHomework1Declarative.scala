package com.evolutiongaming.bootcamp.effects

import scala.concurrent.Future
import scala.util.Try
import scala.annotation.tailrec

/*
 * Homework 1. Provide your own implementation of a subset of `IO` functionality.
 *
 * Provide also tests for this functionality in EffectsHomework1Spec (which you should create).
 *
 * Refer to:
 *  - https://typelevel.org/cats-effect/datatypes/io.html
 *  - https://typelevel.org/cats-effect/api/cats/effect/IO$.html
 *  - https://typelevel.org/cats-effect/api/cats/effect/IO.html
 * about the meaning of each method as needed.
 *
 * There are two main ways how to implement IO:
 * - Executable encoding  - express every constructor and operator for our model in terms of its execution
 * - Declarative encoding - express every constructor and operator for our model as pure data in a recursive
 *                          tree structure
 *
 * While the real Cats Effect IO implementation uses declarative encoding, it will be easier to solve this
 * task using executable encoding, that is:
 *  - Add a `private val run: () => A` parameter to the class `IO` private constructor
 *  - Have most of the methods return a `new IO(...)`
 *
 * Ask questions in the bootcamp chat if stuck on this task.
 */
object EffectsHomework1Declarative {
  implicit val ec = scala.concurrent.ExecutionContext.global

  final private case class Pure[A](a: A) extends IO[A]
  final private case class Delay[A](thunk: () => A) extends IO[A]
  final private case class Suspend[A](thunk: () => IO[A]) extends IO[A]
  final private case class FlatMap[A, B](source: IO[A], f: A => IO[B]) extends IO[B]
  final private case class Map[A, B](source: IO[A], f: A => B) extends IO[B]
  final private case class Attempt[A](source: IO[A]) extends IO[Either[Throwable, A]]

  sealed trait IO[A] {

    sealed trait Computation[A] {
      def flatMap[B](f: A => Computation[B]) = Cont(this, f)
      def map[B](f: A => B): Computation[B] = flatMap(a => More(() => Done(f(a))))
    }
    case class Done[A](result: A) extends Computation[A]
    case class More[A](next: () => Computation[A]) extends Computation[A]
    case class Cont[A, B](next: Computation[A], f: A => Computation[B]) extends Computation[B]

    def step[A](in: IO[A]): Computation[A] = in match {
      case Pure(a)        => Done(a)
      case Delay(thunk)   => Done(thunk())
      case Suspend(thunk) => More(() => step(thunk()))
      case Map(io, f)     => More(() => step(io).map(f))
      case FlatMap(io, f) => More(() => step(io).flatMap(x => step(f(x))))
      case Attempt(io)    => More(() => Try(step(io)).fold(x => Done(Left(x)), _.map(Right(_))))
      // case Map(io, f)     => Cont(More(() => step(io)), (x: Any) => Done(f(x)))
      // case FlatMap(io, f) => Cont(More(() => step(io)), (x: Any) => step(f(x)))
      // case Attempt(io) => More(() => Try(step(io)).fold(x => Done(Left(x)), Cont(_, (x: Any) => Done(Right(x)))))
    }

    def run(): A = {
      val tryOfEvaluation = Try(evaluate(step(this)))
      this match {
        case _: Attempt[A] => tryOfEvaluation.fold(Left(_).asInstanceOf[A], identity)
        case _             => tryOfEvaluation.get
      }
    }

    @tailrec
    private def evaluate(value: Computation[A]): A = value match {
      case Done(result) => result
      case More(next)   => evaluate(next())
      case Cont(next, f) =>
        evaluate {
          next match {
            case Done(result)    => f(result)
            case More(after)     => after().flatMap(f)
            case Cont(after, f2) => after.flatMap(f2(_).flatMap(f))
          }
        }
    }

    def map[B](f: A => B): IO[B] = Map(this, f)
    def flatMap[B](f: A => IO[B]): IO[B] = FlatMap(this, f)
    def *>[B](another: IO[B]): IO[B] = flatMap(_ => another)
    def as[B](newValue: => B): IO[B] = map(_ => newValue)
    def void: IO[Unit] = map(_ => ())
    def attempt: IO[Either[Throwable, A]] = Attempt(this)
    def option: IO[Option[A]] = attempt.map(_.toOption)
    def handleErrorWith[AA >: A](f: Throwable => IO[AA]): IO[AA] = attempt.flatMap(_.fold(f, IO(_)))
    def redeem[B](recover: Throwable => B, map: A => B): IO[B] = attempt.map(_.fold(recover, map))
    def redeemWith[B](recover: Throwable => IO[B], bind: A => IO[B]): IO[B] = attempt.flatMap(_.fold(recover, bind))
    def unsafeRunSync(): A = run()
    def unsafeToFuture(): Future[A] = Future { run() }
  }

  object IO {
    def apply[A](body: => A): IO[A] = delay(body)
    def suspend[A](thunk: => IO[A]): IO[A] = Suspend(() => thunk)
    def delay[A](body: => A): IO[A] = Delay(() => body)
    def pure[A](a: A): IO[A] = Pure(a)
    def fromEither[A](e: Either[Throwable, A]): IO[A] = e.fold(raiseError, pure)
    def fromOption[A](option: Option[A])(orElse: => Throwable): IO[A] = option.fold(raiseError(orElse): IO[A])(pure)
    def fromTry[A](t: Try[A]): IO[A] = t.fold(raiseError, pure)
    def none[A]: IO[Option[A]] = pure(None)
    def raiseError[A](e: Throwable): IO[A] = IO(throw e)
    def raiseUnless(cond: Boolean)(e: => Throwable): IO[Unit] = unlessA(cond)(raiseError(e))
    def raiseWhen(cond: Boolean)(e: => Throwable): IO[Unit] = whenA(cond)(raiseError(e))
    def unlessA(cond: Boolean)(action: => IO[Unit]): IO[Unit] = if (cond) unit else action
    def whenA(cond: Boolean)(action: => IO[Unit]): IO[Unit] = if (cond) action else unit
    val unit: IO[Unit] = pure(())
  }
}
