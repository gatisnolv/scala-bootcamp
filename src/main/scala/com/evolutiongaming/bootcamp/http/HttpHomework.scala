package com.evolutiongaming.bootcamp.http

import cats.effect.{Blocker, ExitCode, IO, IOApp}
import org.http4s._
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.client.dsl.io._
import cats.syntax.all._
import cats.data.NonEmptyList
import org.http4s.headers._
import scala.concurrent.ExecutionContext
import java.util.UUID
import scala.util.Random
import com.evolutiongaming.bootcamp.effects.SharedStateHomework.Cache

import scala.concurrent.duration._
import org.http4s.client.Client

// Homework. Place the solution under `http` package in your homework repository.
//
// Write a server and a client that play a number guessing game together.
//
// Communication flow should be as follows:
// 1. The client asks the server to start a new game by providing the minimum and the maximum number that can
//    be guessed, as well as the maximum number of attempts.
// 2. The server comes up with some random number within the provided range.
// 3. The client starts guessing the number. Upon each attempt, the server evaluates the guess and responds to
//    the client, whether the current number is lower, greater or equal to the guessed one.
// 4. The game ends when the number is guessed or there are no more attempts left. At this point the client
//    should terminate, while the server may continue running forever.
// 5. The server should support playing many separate games (with different clients) at the same time.
//
// Use HTTP or WebSocket for communication. The exact protocol and message format to use is not specified and
// should be designed while working on the task.

case class GameSettings(min: Int, max: Int, tries: Int)
object GuessServer1 extends IOApp {

  private def printLine(string: String = ""): IO[Unit] = IO(println(string))

  case class Game(number: Int, triesLeft: Int, won: Boolean)

  private[http] def httpApp(cache: Cache[IO, String, Game]) = {
    val gameRoutes = {
      import io.circe.generic.auto._
      import org.http4s.circe.CirceEntityCodec._
      HttpRoutes.of[IO] {

        case GET -> Root =>
          Ok(s"Start the game by sending a POST request to /new with json object with fields min, max, tries")

        case req @ POST -> Root / "new" =>
          for {
            settings <- req.as[GameSettings]
            GameSettings(min, max, tries) = settings
            uuid <- IO(UUID.randomUUID().toString)
            number <- IO(Random.between(min, max + 1))
            _ <- cache.put(uuid, Game(number, tries, false))

            // informational
            _ <- printLine(s"numberToGuess: ${number}")
            _ <- printLine(s"min: ${min}, max: ${max}, tries: ${tries}")

            response <- Ok(s"Continue the game by sending POST requests with your guess in the body to /guess. min: ${min}, max:${max}, number of tries:${tries}").map(
              _.addCookie(ResponseCookie("uuid", uuid))
            )
          } yield response
      }
    }

    val guessRoute = HttpRoutes.of[IO] {
      case req @ POST -> Root / "guess" => {

        val uuidIO = for {
          uuid <- req.cookies.find(_.name == "uuid").fold(IO.raiseError(new Exception("uuid header not present")): IO[String])(v => IO.pure(v.content))
        } yield uuid

        val guessIO = for {
          guess <- req.as[String]
          res <- guess.toIntOption.fold(IO.raiseError(new Exception("Guess was not an int")): IO[Int])(n => IO.pure(n))
        } yield res

        (for {
          uuid <- uuidIO
          guessString <- req.as[String]
          guess <- guessIO
          game <- cache.get(uuid)
          response <- game match {
            case Some(game) =>
              game match {
                case Game(number, triesLeft, false) if triesLeft > 0 =>
                  if (number == guess) {
                    cache.put(uuid, Game(number, triesLeft - 1, true)) *>
                      Ok(s"You won, the number is ${number}!")
                  } else {
                    cache.put(uuid, Game(number, triesLeft - 1, false)) *>
                      Ok(s"Wrong guess, tries left: ${triesLeft - 1}. The number is ${if (number > guess) "greater" else "lower"}.")
                  }
                case Game(number, _, true) => Ok(s"You already won the game, the number was ${number}!")
                case _                     => Ok("Sorry, you lost, no more tries left. You may start a new game.")
              }
            case None => BadRequest("Game with uuid not found (or expired). You may start a new game.")
          }
        } yield response).handleErrorWith(e => BadRequest(e.getMessage))
      }
    }

    gameRoutes <+> guessRoute
  }.orNotFound

  override def run(args: List[String]): IO[ExitCode] = for {
    cache <- Cache.of[IO, String, Game](10.minutes, 2.minutes)
    _ <- BlazeServerBuilder[IO](ExecutionContext.global)
      .bindHttp(port = 9001, host = "localhost")
      .withHttpApp(httpApp(cache))
      .serve
      .compile
      .drain
  } yield ExitCode.Success
}

object GuessClient1 extends IOApp {

  private val uri = uri"http://localhost:9001"

  private val minv = Int.MinValue / 2
  private val maxv = Int.MaxValue / 2
  private val min = Random.between(minv, maxv)
  private val max = Random.between(min, maxv)
  private val neededTries = Math.floor((Math.log(max - min) / Math.log(2)) + 1).toInt

  private def printLine(string: String = ""): IO[Unit] = IO(println(string))

  def initializeGame(client: Client[IO]) = {
    import io.circe.generic.auto._
    import org.http4s.circe.CirceEntityCodec._
    for {
      r <- Method.POST(GameSettings(min, max, neededTries), uri / "new")
      id <- {
        client.run(r).use {
          case Status.Successful(r) =>
            r.cookies.find(_.name == "uuid") match {
              case Some(cookie) => IO.pure(cookie.content)
              case None         => IO.raiseError(new Exception("uuid cookie was not returned"))
            }
          case r =>
            r.as[String].flatMap(b => IO.raiseError(new Exception(s"Requesst failed with status ${r.status.code}and body $b")))
        }
      }

      // informational
      _ <- printLine(s"Attempting to guess in $neededTries tries")
      _ <- printLine("uuid cookie: " + id)
    } yield id
  }

  def playGame(client: Client[IO], id: String, guess: Int, min: Int, max: Int): IO[String] = {
    val won = ".*won.*".r
    val lost = ".*lost.*".r
    val greater = ".*greater.*".r
    val lower = ".*lower.*".r
    for {
      attemptResult <- client.expect[String](Method.POST(guess.toString, uri / "guess").map(_.addCookie("uuid", id)))
      _ <- printLine(s"attempted ${guess}, min: ${min}, max ${max}")
      _ <- printLine(attemptResult)
      result <- attemptResult match {
        case won()     => IO.pure(attemptResult)
        case lost()    => IO.pure(attemptResult)
        case greater() => playGame(client, id, Math.ceil((max.toDouble + guess) / 2).toInt, guess + 1, max)
        case lower()   => playGame(client, id, Math.floor((min.toDouble + guess) / 2).toInt, min, guess - 1)
        case _         => IO.pure("Unexpected result" + attemptResult)
      }
    } yield result
  }

  def run(args: List[String]): IO[ExitCode] = BlazeClientBuilder[IO](ExecutionContext.global).resource
    .parZip(Blocker[IO])
    .use { case (client, blocker) =>
      (for {
        id <- initializeGame(client)
        _ <- playGame(client, id, (max + min) / 2, min, max)
      } yield ()).handleErrorWith(e => printLine(s"Error: ${e.getMessage}"))
    }
    .as(ExitCode.Success)
}
