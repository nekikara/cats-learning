package sandbox

trait CCodec[A] {
  self =>
  def encode(value: A): String
  def decode(value: String): A
  def imap[B](dec: A => B, enc: B => A): CCodec[B] = {
    new CCodec[B] {
      override def encode(value: B): String = self.encode(enc(value))
      override def decode(value: String): B = dec(self.decode(value))
    }
  }
}

object CCodec {
  implicit val stringCodec: CCodec[String] =
    new CCodec[String] {
      override def encode(value: String): String = value
      override def decode(value: String): String = value
    }

  implicit val doubleCodec: CCodec[Double] = stringCodec.imap(_.toDouble, _.toString)
  implicit val intCodec: CCodec[Int] = stringCodec.imap(_.toInt, _.toString)

  def encode[A](value: A)(implicit c: CCodec[A]): String = c.encode(value)
  def decode[A](value: String)(implicit c: CCodec[A]): A = c.decode(value)
}

