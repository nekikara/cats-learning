package sandbox


object Main extends App {
  import cats.data.Validated
  import cats.instances.list._
  import cats.syntax.either._
  import cats.syntax.apply._

  type FormData = Map[String, String]
  type FailFast[A] = Either[List[String], A]
  type FailSlow[A] = Validated[List[String], A]

  def getValue(name: String)(data: FormData): FailFast[String] =
    data.get(name).toRight(List(s"$name field not specified"))

  def parseInt(str: String): FailFast[Int] =
    util.Try(str.toInt).toEither match {
      case Right(x) => Right(x)
      case Left(_) => Left(List(s"$str can't be parsed"))
    }
  def nonBlank(value: String): FailFast[Boolean] =
    if (value.nonEmpty) Right(true) else Left(List(s"$value is blank!"))
  def nonNegative(value: Int): FailFast[Boolean] =
    if (value > 0) {
      Right(true)
    } else {
      Left(List(s"$value is negative"))
    }

  def readName(data: FormData): FailFast[String] =
    for {
      name <- getValue("name")(data)
      _ <- nonBlank(name)
    } yield name

  def readAge(data: FormData): FailFast[Int] =
    for {
      age <- getValue("age")(data)
      n <- parseInt(age)
      _ <- nonNegative(n)
    } yield n

  case class User(name: String, age: Int)

  def validatedUser(data: FormData): FailSlow[User] =
    (
      readName(data).toValidated,
      readAge(data).toValidated
    ).mapN(User.apply)

  val validData = Map(
    "name" -> "hogeohge",
    "age" -> "32"
  )
  println(validatedUser(validData))
  val blankName = Map(
    "name" -> "",
    "age" -> "32"
  )
  println(validatedUser(blankName))
  val negativeAge = Map(
    "name" -> "",
    "age" -> "-32"
  )
  println(validatedUser(negativeAge))
  val noNameKey = Map(
    "nam" -> "",
    "age" -> "-32"
  )
  println(validatedUser(noNameKey))
  val noNumericAge = Map(
    "name" -> "sxxx",
    "age" -> "asdie"
  )
  println(validatedUser(noNameKey))
}
