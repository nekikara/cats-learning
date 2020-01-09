package sandbox

import cats.Show
import cats.Eq
import cats.instances.int._
import cats.instances.string._
import cats.syntax.show._
import cats.syntax.eq._

final case class Cat(name: String, age: Int, color: String)

object Cat {
  implicit val catShow: Show[Cat] = Show.show[Cat] { cat =>
    val name = cat.name.show
    val age = cat.age.show
    val color = cat.color.show
    s"$name is a $age year-old $color cat."
  }

  implicit val catEq: Eq[Cat] = Eq.instance[Cat] { (c1, c2) =>
    (c1.name === c2.name) & (c1.age === c2.age) & (c1.color === c2.color)
  }
}

