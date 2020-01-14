package sandbox

import cats.data.EitherT
import cats.instances.future._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

object Main extends App {
  type Response[A] = EitherT[Future, String, A]

  val powerLevels = Map(
    "Jazz" -> 6,
    "Bumblebee" -> 8,
    "Hot Rod" -> 10
  )
  def getPowerLevel(autobot: String): Response[Int] =
    powerLevels.get(autobot) match {
      case Some(p) => EitherT.right(Future(p))
      case None => EitherT.left(Future(s"$autobot unreachable"))
    }

  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] =
    for {
      a <- getPowerLevel(ally1)
      b <- getPowerLevel(ally2)
    } yield 15 < (a + b)

  def tacticalReport(ally1: String, ally2: String): String = {
    val result = canSpecialMove(ally1, ally2).value
    Await.result(result, 1.second) match {
      case Right(false) => s"$ally1 and $ally2 need a recharge"
      case Right(true) => s"$ally1 and $ally2 are ready to roll out!"
      case Left(msg) => s"Comms error: $msg"
    }
  }

  println(Await.result(getPowerLevel("Jazz").value, 1.second))
  println(Await.result(getPowerLevel("hogheoge").value, 1.second))
  println(Await.result(canSpecialMove("Jazz", "Hot Rod").value, 1.second))
  println(Await.result(canSpecialMove("Jazz", "Bumblebee").value, 1.second))
  println(tacticalReport("Jazz", "Bumblebee"))
  println(tacticalReport("Bumblebee", "Hot Rod"))
  println(tacticalReport("Jazz", "Ironhide"))
}
