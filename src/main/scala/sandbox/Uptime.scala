package sandbox

import scala.concurrent.Future
import cats.Applicative
import cats.instances.list._
import cats.syntax.functor._
import cats.syntax.traverse._

object Uptime {
  trait UptimeClient[F[_]] {
    def getUptime(hostname: String): F[Int]
  }

  class UptimeService[F[_]: Applicative](client: UptimeClient[F]) {
    def getTotalUptime(hostnames: List[String]): F[Int] =
      hostnames.traverse(client.getUptime).map(_.sum)
  }

  trait RealUptimeClient extends UptimeClient[Future] {
    override def getUptime(hostname: String): Future[Int]
  }
}
