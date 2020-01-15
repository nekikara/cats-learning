package sandbox

object Main extends App {
  val left = List(1, 2, 3, 4).foldLeft(List.empty[Int])((acc, i) => i :: acc)
  println(left)
  val right = List(1, 2, 3, 4).foldRight(List.empty[Int])((acc, i) => acc :: i)
  println(right)

  def map[A, B](list: List[A])(f: A => B): List[B] =
    list.foldRight(List.empty[B])((i, acc) => f(i) :: acc)
  println(map(List(1, 2, 3))(_ * 2))

  def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] =
    list.foldRight(List.empty[B])((i, acc) => f(i) ::: acc)
  println(flatMap(List(1, 2, 3))(a =>  List(a * 2, a * 4, a * 6)))

  def filter[A](list: List[A])(f: A => Boolean): List[A] =
    list.foldRight(List.empty[A])((i, acc) =>  if (f(i)) i :: acc else acc )
  println(filter(List(1, 2, 3))(2 <= _))

  import scala.math.Numeric
  def sum[A](list: List[A])(implicit numeric: Numeric[A]): A =
    list.foldRight(numeric.zero)(numeric.plus)

  println(sum(List(1, 2, 3)))

  import cats.Monoid
  import cats.instances.int._

  def sumWithMonoid[A](list: List[A])(implicit monoid: Monoid[A]): A =
    list.foldRight(monoid.empty)(monoid.combine)
  println(sumWithMonoid(List(4, 5, 6)))
}
