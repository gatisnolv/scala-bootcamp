package com.evolutiongaming.bootcamp.basics

import scala.io.Source
import com.evolutiongaming.bootcamp.basics.ControlStructuresHomework.Command._

object ControlStructuresHomework {
  // Homework

  // Create a command line application that reads various "commands" from the
  // stdin, evaluates them, and writes output to stdout.

  // Commands are:

  //   divide 4 5
  // which should output "4 divided by 5 is 0.8"

  //   sum 5 5 6 8.5
  // which should output "the sum of 5 5 6 8.5 is 24.5"

  //   average 4 3 8.5 4
  // which should output "the average of 4 3 8.5 4 is 4.875"

  //   min 4 -3 -17
  // which should output "the minimum of 4 -3 -17 is -17"

  //   max 4 -3 -17
  // which should output "the maximum of 4 -3 -17 is 4"

  // In case of commands that cannot be parsed or calculations that cannot be performed,
  // output a single line starting with "Error: "

  sealed trait Command
  object Command {
    final case class Divide(dividend: Double, divisor: Double) extends Command
    final case class Sum(numbers: List[Double]) extends Command
    final case class Average(numbers: List[Double]) extends Command
    final case class Min(numbers: List[Double]) extends Command
    final case class Max(numbers: List[Double]) extends Command
  }

  final case class ErrorMessage(value: String)

  // Adjust `Result` and `ChangeMe` as you wish - you can turn Result into a `case class` and remove the `ChangeMe` if
  // you think it is the best model for your solution, or just have other `case class`-es implement `Result`
  sealed trait Result
  final case class DivisionResult(dividend: Double, divisor: Double, value: Double) extends Result
  final case class SumResult(numbers: List[Double], value: Double) extends Result
  final case class AverageResult(numbers: List[Double], value: Double) extends Result
  final case class MinResult(numbers: List[Double], value: Double) extends Result
  final case class MaxResult(numbers: List[Double], value: Double) extends Result

  def parseCommand(x: String): Either[ErrorMessage, Command] = {
    val splitResult: List[String] = x.stripLeading.stripTrailing.stripLineEnd.split("\\s+").toList
    val tokens = if (splitResult.length == 1 && splitResult(0) == "") Nil else splitResult // used to handle exclusively whitespace inputs

    tokens match {
      case x :: xs =>
        val parameters: List[Option[Double]] = xs.map(x => x.toDoubleOption)
        if (parameters.contains(None)) {
          Left(ErrorMessage(s"Non-numeric inputs found. All commands operate on numeric inputs."))
        } else {
          val inputs = parameters.flatten
          x match {
            case "divide"  => if (xs.length == 2) Right(Divide(inputs(0), inputs(1))) else Left(ErrorMessage("Wrong number of parameters for division."))
            case "sum"     => Right(Sum(inputs))
            case "average" => Right(Average(inputs))
            case "min"     => Right(Min(inputs))
            case "max"     => Right(Max(inputs))
            case unknown => {
              Left(ErrorMessage(s"Unsupported command: '$unknown'"))
            }
          }
        }
      case Nil => Left(ErrorMessage("Empty input"))
    }
  }

  // should return an error (using `Left` channel) in case of division by zero and other
  // invalid operations
  def calculate(x: Command): Either[ErrorMessage, Result] = {
    x match {
      case Divide(dividend, divisor) => if (divisor != 0) Right(DivisionResult(dividend, divisor, dividend / divisor)) else Left(ErrorMessage("division by 0"))
      case Sum(numbers)              => if (numbers != Nil) Right(SumResult(numbers, numbers.sum)) else Left(ErrorMessage("no numbers provided"))
      case Average(numbers)          => if (numbers != Nil) Right(AverageResult(numbers, numbers.sum / numbers.size)) else Left(ErrorMessage("no numbers provided"))
      case Min(numbers)              => if (numbers != Nil) Right(MinResult(numbers, numbers.min)) else Left(ErrorMessage("no numbers provided"))
      case Max(numbers)              => if (numbers != Nil) Right(MaxResult(numbers, numbers.max)) else Left(ErrorMessage("no numbers provided"))
    }
  }

  def renderResult(x: Result): String = {
    x match {
      case DivisionResult(dividend, divisor, value) => s"$dividend divided by $divisor is $value"
      case SumResult(numbers, value)                => s"the sum of ${numbers.toList.mkString(" ")} is $value"
      case AverageResult(numbers, value)            => s"the average of ${numbers.toList.mkString(" ")} is $value"
      case MinResult(numbers, value)                => s"the minimum of ${numbers.toList.mkString(" ")} is $value"
      case MaxResult(numbers, value)                => s"the maximum of ${numbers.toList.mkString(" ")} is $value"
    }
  }

  def renderError(e: ErrorMessage): String = {
    s"Error: ${e.value}"
  }

  def process(x: String): String = {
    (for {
      command <- parseCommand(x)
      result <- calculate(command)
    } yield result) match {
      case Right(result)      => renderResult(result)
      case Left(errorMessage) => renderError(errorMessage)
    }
  }

  // This `main` method reads lines from stdin, passes each to `process` and outputs the return value to stdout
  def main(args: Array[String]): Unit = Source.stdin.getLines() map process foreach println
}
