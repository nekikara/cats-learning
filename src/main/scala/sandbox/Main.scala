package sandbox

import Uptime._
import cats.Id

object Main extends App {
  class TestUptimeClient(hosts: Map[String, Int]) extends UptimeClient[Id] {
    override def getUptime(hostname: String): Id[Int] = hosts.getOrElse(hostname, 0)
  }

  def testTotalUptime() = {
    val hosts = Map("host1" -> 10, "host2" -> 6)
    val client = new TestUptimeClient(hosts)
    val service = new UptimeService(client)
    val actual = service.getTotalUptime(hosts.keys.toList)
    val expected = hosts.values.sum
    assert(actual == expected)
  }

  testTotalUptime()
}
