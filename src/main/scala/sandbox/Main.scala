package sandbox

import cats.Monad

import cats.syntax.functor._
import cats.syntax.flatMap._
import scala.language.higherKinds

import scala.concurrent.ExecutionContext.Implicits.global
import cats.instances.future._
import scala.concurrent._
import scala.concurrent.duration._

import cats.instances.option._
import cats.instances.list._

import cats.syntax.applicative._

object Main extends App {
  type Id[A] = A

  val fm = Monad[Future]
  val future = fm.flatMap(fm.pure(1))(x => fm.pure(x + 2))
  Await.result(future, 10.second)

  1.pure[Option]
  1.pure[List]

  def sumSquare[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] =
    for {
      x <- a
      y <- b
    } yield x * x + y * y

  println(sumSquare(Option(3), Option(4)))
  println(sumSquare(List(1, 2, 3), List(4, 5)))
  println(sumSquare(3: Id[Int], 4: Id[Int]))

  println(CId.pure(123))
  println(CId.map(123)(_ * 2))
  println(CId.flatMap(123)(_ * 2))
}
