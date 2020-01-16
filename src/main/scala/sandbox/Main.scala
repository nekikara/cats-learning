package sandbox

import cats.kernel.Monoid
import cats.syntax.semigroup._
import cats.instances.future._
import cats.instances.int._

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global


object Main extends App {
  def foldMap[A, B: Monoid](vec: Vector[A])(fn: A => B): B =
    vec.foldLeft(Monoid.empty[B])(_ |+| fn(_))

  def parallelFoldMap[A, B: Monoid](vals: Vector[A])(fn: A => B): Future[B] = {
    val cpus = Runtime.getRuntime.availableProcessors()
    val groupSize = (1.0 * vals.size / cpus).ceil.toInt
    val batches = vals.grouped(groupSize).toList
    val futures = batches.map(batch => Future {
      foldMap(batch)(fn)
    })
    Monoid[Future[B]].combineAll(futures)
  }

  val f1 = parallelFoldMap((1 to 10000).toVector)(identity)
  val ranF1 = Await.result(f1, 1.second)
  println(ranF1)

}
