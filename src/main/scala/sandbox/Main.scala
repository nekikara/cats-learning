package sandbox

import cats.kernel.Monoid
import cats.instances.int._
import cats.instances.option._
import cats.syntax.semigroup._

object Main extends App {
  def add[A](items: List[A])(implicit monoid: Monoid[A]): A =
    items.foldLeft(monoid.empty)(_ |+| _)

  println(add(List(Some(100), Some(1), None, Some(50))))
  println(add(List(Order(1, 2), Order(3, 4))))
}
