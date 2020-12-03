package com.evolutiongaming.bootcamp.cats.v2

import scala.concurrent.Future

object p7_Traverse {
  import scala.concurrent.ExecutionContext.Implicits.global

  /**
    * Traverse provides us a tool for convenient iteration.
    * To grasp the concept, let's take a look at the following example
    */
  final case class User(name: String)
  // imagine that we call some remote API
  def fetchUser(name: String): Future[User] = Future.successful(User(name))
  val userNames = List("Bob", "Alice", "Ann", "Apu")

  /**
    * Given a list of user names, we query users by name from some remote resource.
    * If we simply map over a list we'll end up with a list of futures, which is not very useful usually.
    */
  val listOfFutures: List[Future[User]] = userNames.map(fetchUser)

  /**
    * Instead, let's make use of Future.traverse method thus we'll have a future of a list of users.
    */
  val users: Future[List[User]] = Future.traverse(userNames)(fetchUser)

  /**
    * Ex 7.0 implement traverse function for Option
    */

  def optionTraverse[A](input: List[Option[A]]): Option[List[A]] = {
    input match {
      case Nil       => Some(Nil)
      case None :: _ => None
      case Some(v) :: xs =>
        optionTraverse(xs) match {
          case Some(list) => Some(v :: list)
          case _          => None
        }
    }
  }

  def optionTraverse2[A](input: List[Option[A]]): Option[List[A]] = {
    def iter(list: List[Option[A]], acc: List[A]): Option[List[A]] = list match {
      case Nil           => Some(acc)
      case None :: _     => None
      case Some(v) :: xs => iter(xs, acc :+ v)
    }
    iter(input, Nil)
  }

  /**
    * Ex 7.1 implement traverse for Either. Use fail fast approach (the first error encountered is returned.)
    */
  def eitherTraverse[E, A](input: List[Either[E, A]]): Either[E, List[A]] = {
    def iter(list: List[Either[E, A]], acc: List[A]): Either[E, List[A]] = {
      list match {
        case Nil            => Right(acc)
        case Left(e) :: _   => Left(e)
        case Right(v) :: xs => iter(xs, acc :+ v)
      }
    }
    iter(input, Nil)
  }

  // As usual we can find some instances defined for standard types
  import cats.instances.list._
  import cats.instances.option._
  import cats.syntax.traverse._

  val listOfFlags = List(true, false, false, true)

  val options: List[Option[String]] =
    listOfFlags.map(b => if (b) Some("Good") else None)

  val strings: Option[List[String]] =
    options.traverse(identity)
}
