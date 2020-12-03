package com.evolutiongaming.bootcamp.effects

import cats.Monad
import cats.effect.concurrent.Ref
import cats.effect.{Clock, Concurrent, ExitCode, IO, IOApp, Timer}
import cats.implicits._

import scala.concurrent.duration._

/*
 * Please implement a Cache which allows concurrent access.
 *
 * Tip: checking expiration could be represented as some infinite process somewhere in background
 *
 * Tip: you can use following structure to get current time suspended in effect : Clock[F].realTime(MILLISECONDS).flatMap(...)
 *
 * Cached items should have an expiration timestamp after which they are evicted.
 *
 * If we will put a value with the same key then it should renew expiration
 */
object SharedStateHomework extends IOApp {

  trait Cache[F[_], K, V] {
    def get(key: K): F[Option[V]]

    def put(key: K, value: V): F[Unit]
  }

  class RefCache[F[_]: Clock: Monad, K, V](state: Ref[F, Map[K, (Long, V)]], expiresIn: FiniteDuration) extends Cache[F, K, V] {

    def get(key: K): F[Option[V]] = for {
      map <- state.get
    } yield map.get(key).map { case (_, value) => value }

    def put(key: K, value: V): F[Unit] = for {
      time <- Clock[F].realTime(MILLISECONDS)
      _ <- state.update(_.updated(key, (time, value)))
    } yield ()
  }

  object Cache {
    def of[F[_]: Clock, K, V](expiresIn: FiniteDuration, checkOnExpirationsEvery: FiniteDuration)(implicit T: Timer[F], C: Concurrent[F]): F[Cache[F, K, V]] = {

      def cleanExpiredEntries(state: Ref[F, Map[K, (Long, V)]]) = for {
        time <- Clock[F].realTime(MILLISECONDS)
        _ <- state.update(_.filter { case (_, (timeCached, _)) => time - timeCached < expiresIn.toMillis })
      } yield ()

      for {
        state <- Ref.of[F, Map[K, (Long, V)]](Map.empty)
        _ <- C.start((T.sleep(checkOnExpirationsEvery) *> cleanExpiredEntries(state)).foreverM.void)
      } yield new RefCache(state, expiresIn)
    }
  }

  override def run(args: List[String]): IO[ExitCode] = {

    for {
      cache <- Cache.of[IO, Int, String](10.seconds, 4.seconds)
      _ <- cache.put(1, "Hello")
      _ <- cache.put(2, "World")
      _ <- cache.get(1).flatMap(s => IO { println(s"first key $s") })
      _ <- cache.get(2).flatMap(s => IO { println(s"second key $s") })
      _ <- IO.sleep(12.seconds)
      _ <- cache.get(1).flatMap(s => IO { println(s"first key $s") })
      _ <- cache.get(2).flatMap(s => IO { println(s"second key $s") })
      _ <- IO.sleep(12.seconds)
      _ <- cache.get(1).flatMap(s => IO { println(s"first key $s") })
      _ <- cache.get(2).flatMap(s => IO { println(s"second key $s") })
    } yield ExitCode.Success
  }
}
