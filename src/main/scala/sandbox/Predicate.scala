package sandbox

import cats.Semigroup
import cats.data.Validated
import cats.data.Validated._
import cats.syntax.apply._
import cats.syntax.semigroup._

sealed trait Predicate[E, A] {
  def apply(value: A)(implicit s: Semigroup[E]): Validated[E, A] =
    this match {
      case PureP(func) => func(value)
      case And(left, right) =>
        (left(value), right(value)).mapN((_, _) => value)
      case Or(left, right) =>
        left(value) match {
          case Valid(value) => Valid(value)
          case Invalid(e1) =>
            right(value) match {
              case Valid(value) => Valid(value)
              case Invalid(e2) => Invalid(e1 |+| e2)
            }
        }
    }

  def and(that: Predicate[E, A]): Predicate[E, A] = And(this, that)
  def or(that: Predicate[E, A]): Predicate[E, A] = Or(this, that)
}
final case class And[E, A](left: Predicate[E, A], right: Predicate[E, A]) extends Predicate[E, A]
final case class Or[E, A](left: Predicate[E, A], right: Predicate[E, A]) extends Predicate[E, A]
final case class PureP[E, A](func: A => Validated[E, A]) extends Predicate[E, A]
