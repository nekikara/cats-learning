package sandbox

import Monoid._

object Main extends App {
  def associativeLaw[A](x: A, y: A, z: A)(implicit m: Monoid[A]): Boolean = {
    m.combine(x, m.combine(y, z)) == m.combine(m.combine(x, y), z)
  }
  def identityLaw[A](x: A)(implicit m: Monoid[A]): Boolean = {
    (m.combine(x, m.empty) == x) && (m.combine(m.empty, x) == x)
  }

  println(associativeLaw[Boolean](true, true, false))
  println(associativeLaw[Boolean](false, true, false))
  println(identityLaw[Boolean](true))
  println(identityLaw[Boolean](false))
}
