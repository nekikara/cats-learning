package sandbox

trait Semigroup[A] {
  def combine(x: A, y: A): A
}

trait Monoid[A] extends Semigroup[A] {
  def empty: A
}

object Monoid {
  def apply[A](implicit monoid: Monoid[A]): Monoid[A] = monoid

//  implicit val booleanAndMonoid: Monoid[Boolean] = new Monoid[Boolean] {
//    override def combine(x: Boolean, y: Boolean): Boolean = x && y
//    override def empty: Boolean = true
//  }
//  implicit val booleanOrMonoid: Monoid[Boolean] = new Monoid[Boolean] {
//    override def combine(x: Boolean, y: Boolean): Boolean = x || y
//    override def empty: Boolean = false
//  }
//  implicit val booleanEitherMonoid: Monoid[Boolean] = new Monoid[Boolean] {
//    override def combine(x: Boolean, y: Boolean): Boolean = (x && !y) || (!x && y)
//    override def empty: Boolean = false
//  }
  implicit val booleanXnorMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    override def combine(x: Boolean, y: Boolean): Boolean = (!x || y) && (x || !y)
    override def empty: Boolean = true
  }
}


