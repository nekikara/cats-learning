package sandbox

import cats.syntax.semigroup._
import cats.kernel.CommutativeMonoid
import cats.instances.map._

trait GCounter[F[_, _], K, V] {
  def increment(f: F[K, V])(k: K, v: V)(implicit m: CommutativeMonoid[V]): F[K, V]
  def merge(f1: F[K, V], f2: F[K, V])(implicit b: BoundedSemiLattice[V]): F[K, V]
  def total(f: F[K, V])(implicit i: CommutativeMonoid[V]): V
}

object GCounter {
  def apply[F[_, _], K, V](implicit counter: GCounter[F, K, V]): GCounter[F, K, V] =
    counter

  implicit def mapInstance[K, V]: GCounter[Map, K, V] = new GCounter[Map, K, V] {
    override def increment(f: Map[K, V])(k: K, v: V)
                          (implicit m: CommutativeMonoid[V]): Map[K, V] =
      f + (k -> m.combine(v, f.getOrElse(k, m.empty)))

    override def merge(f1: Map[K, V], f2: Map[K, V])
                      (implicit b: BoundedSemiLattice[V]): Map[K, V] =
      f1 |+| f2

    override def total(f: Map[K, V])(implicit i: CommutativeMonoid[V]): V = i.combineAll(f.values)
  }
}

