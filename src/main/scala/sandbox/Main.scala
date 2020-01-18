package sandbox

import GCounter._
import cats.instances.int._

object Main extends App {
  val g1 = Map("a" -> 7, "b" -> 3)
  val g2 = Map("a" -> 2, "b" -> 5)

  val counter = GCounter[Map, String, Int]
  val incremented = counter.increment(g1)("a", 100)
  println(incremented)
  val merged = counter.merge(g1, g2)
  println(merged)
  val total = counter.total(merged)
  println(total)
}
