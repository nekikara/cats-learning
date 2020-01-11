package sandbox

import cats.syntax.functor._
import PrintableSyntax._
import PrintableInstances._

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

  val printable = Printable.contramap((a: Int) => s"a = $a")
  1000.print(printable)
  Printable.print(CBox("yyyyy"))
}
