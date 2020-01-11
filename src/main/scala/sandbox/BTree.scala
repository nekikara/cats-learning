package sandbox

import cats.Functor

sealed trait BTree[+A]
final case class Branch[A](left: BTree[A], right: BTree[A]) extends BTree[A]
final case class Leaf[A](value: A) extends BTree[A]

object BTree {
  def branch[A](left: BTree[A], right: BTree[A]): BTree[A] = Branch(left, right)
  def leaf[A](value: A): BTree[A] = Leaf(value)

  implicit val btreeFunctor: Functor[BTree] =
    new Functor[BTree] {
      override def map[A, B](fa: BTree[A])(f: A => B): BTree[B] =
        fa match {
          case Branch(left, right) => Branch(map(left)(f), map(right)(f))
          case Leaf(value) => Leaf(f(value))
        }
    }
}
