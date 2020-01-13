package sandbox

import cats.data.State
import cats.syntax.applicative._

object Main extends App {
  val step1 = State[Int, String] { state =>
    val ans = state + 1
    (ans, s"$state + 1 = Result of step1: $ans\n")
  }

  val step2 = State[Int, String] { num =>
    val ans = num * 2
    (ans, s"$num * 2 = Result of step2: $ans\n")
  }

  val both = for {
    a <- step1
    b <- step2
  } yield (a, b)

  val (state, result) = both.run(100).value
//  println(state)
//  println(result)

  type CalcState[A] = State[List[Int], A]
  def evalOne(sym: String): CalcState[Int] =
    sym match {
      case "+" => operator(_ + _)
      case "-" => operator(_ - _)
      case "/" => operator(_ / _)
      case "*" => operator(_ * _)
      case num => operand(num.toInt)
    }

  def operand(i: Int): CalcState[Int] = State[List[Int], Int] { state =>
    (i :: state, i)
  }

  def operator(function: (Int, Int) => Int): CalcState[Int] =
    State[List[Int], Int] {
      case b :: a :: tail =>
        val ans = function(a, b)
        (ans :: tail, ans)
      case _ =>
        sys.error("Fail!")
    }

  def evalAll(input: List[String]): CalcState[Int] =
    input.foldLeft(0.pure[CalcState]) { (acc, b) =>
      acc.flatMap(_ => evalOne(b))
    }
//    (input: @unchecked) match {
//      case h :: tail => for {
//                          b <- evalOne(h)
//                          ans <- if (tail != Nil) evalAll(tail) else State.pure[List[Int], Int](b)
//                        } yield ans
//    }

  val first = evalOne("42").runA(Nil).value
  println(first)

  val program = for {
    _ <- evalOne("1")
    _ <- evalOne("2")
    ans <- evalOne("+")
  } yield ans
  println(program.runA(Nil).value)

  val program2 = evalAll(List("1", "2", "+", "3", "*"))
  println(s"program2 => ${program2.runA(Nil).value}")

  val program3 = for {
    _ <- evalAll(List("1", "2", "+"))
    _ <- evalAll(List("3", "4", "+"))
    ans <- evalOne("*")
  } yield ans
  println(s"program3 => ${program3.runA(Nil).value}")

  def evalInput(input: String): CalcState[Int] =
    evalAll(input.split(" ").toList)

  val program4 = evalInput("1 2 + 3 4 + *")
  println(s"program4 => ${program4.runA(Nil).value}")
  val program5 = evalInput("1 2 + 3 *")
  println(s"program5 => ${program5.runA(Nil).value}")
}
