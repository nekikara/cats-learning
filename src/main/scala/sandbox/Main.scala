package sandbox

object Main extends App {
  println(CCodec.encode[Double](123.4))
  println(CCodec.decode[Double]("123.4"))

  println(CCodec.decode[CBox[Int]]("21131"))
  println(CCodec.encode[CBox[Double]](CBox(123.4)))
}
