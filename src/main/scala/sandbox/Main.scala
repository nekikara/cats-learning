package sandbox

import cats.syntax.functor._

object Main extends App {
  val f1: Int => Double = (a: Int) => a * 0.5
  val f2: Double => String = (a: Double) => s"$a ___ Int"
  val btree = BTree.branch(
    BTree.leaf(1),
    BTree.branch(
      BTree.branch( BTree.leaf(1), BTree.leaf(100)),
      BTree.leaf(1000)
    )
  )

  println(btree.map(f1).map(f2))
}
