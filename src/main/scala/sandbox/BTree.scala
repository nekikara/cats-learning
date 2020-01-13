package sandbox

import cats.Functor
import cats.Monad

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

  implicit val btreeMonad: Monad[BTree] =
    new Monad[BTree] {
      override def flatMap[A, B](fa: BTree[A])(f: A => BTree[B]): BTree[B] =
        fa match {
          case Leaf(x) => f(x)
          case Branch(left, right) => Branch(flatMap(left)(f), flatMap(right)(f))
        }

      override def pure[A](x: A): BTree[A] = leaf(x)

      override def tailRecM[A, B](a: A)(f: A => BTree[Either[A, B]]): BTree[B] =
        flatMap(f(a)) {
          case Left(value) => tailRecM(value)(f)
          case Right(value) => Leaf(value)
        }
    }
}
