package sandbox

import cats.Show
import cats.implicits._

object Main extends App {
  val showInt: Show[Int] = Show.apply[Int]
  val showString: Show[String] = Show.apply[String]

  val intAsString: String = showInt.show(123)
  val stringAsString: String = showString.show("abc")

  val showIntSynt = 123.show
  val showStringSynt = "abc".show

  val showCat: Show[Cat] = Show.apply[Cat]
  println(showCat.show(Cat("hogehoge", 1230, "Blue")))
  println(Cat("Syntax", 5555, "Red").show)

  val cat1 = Cat("Garfield", 38, "orange and black")
  val cat2 = Cat("Heathcliff", 33, "orange and black")
  println(cat1 === cat2)

  val optionCat1 = Option(cat1)
  val optionCat2 = Option.empty[Cat]
  println(optionCat1 =!= optionCat2)
}
