package sandbox

final case class CBox[A](value: A)

object CBox {
  implicit def cBoxPrintable[A](implicit p: Printable[A]): Printable[CBox[A]] =
    p.contramap[CBox[A]](_.value)
}
