package sandbox

import cats.data.Reader
import cats.syntax.applicative._

object Main extends App {
  val catName: Reader[Cat, String] =
    Reader(cat => cat.name)
  val greetKitty: Reader[Cat, String] =
    catName.map(name => s"Hello $name")

  val feedKitty: Reader[Cat, String] =
    Reader(cat => s"Have a nice bowl of ${cat.name}")

  val greetAndFeed: Reader[Cat, String] =
    for {
      greet <- greetKitty
      feed <- feedKitty
    } yield s"$greet. $feed"

  println(greetAndFeed(Cat("Ken", 35, "blue")))
  println(greetAndFeed(Cat("Maya", 35, "red")))

  /*
   * DbReader
   */
  case class Db(usernames: Map[Int, String], passwords: Map[String, String])
  type DbReader[A] = Reader[Db, A]

  def findUsername(userId: Int): DbReader[Option[String]] =
    Reader(db => db.usernames.get(userId))

  def checkPassword(username: String, password: String): DbReader[Boolean] =
    Reader(db => db.passwords.get(username).contains(password))

  def checkLogin(userId: Int, password: String): DbReader[Boolean] =
    for {
      username <- findUsername(userId)
      result <- username.map { un =>
                  checkPassword(un, password)
                }.getOrElse {
                  false.pure[DbReader]
                }
    } yield result

  val users = Map(
    1 -> "dade",
    2 -> "kate",
    3 -> "margo"
  )

  val passwords = Map(
    "dade" -> "zerocool",
    "kate" -> "acidburn",
    "margo" -> "secret"
  )
  val db = Db(users, passwords)
  println("===== Check Login by using checkLogin =====")
  println(checkLogin(1, "zerocool").run(db))
  println(checkLogin(3, "secret").run(db))
}
