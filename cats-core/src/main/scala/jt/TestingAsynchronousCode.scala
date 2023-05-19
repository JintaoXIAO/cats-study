package jt

import scala.concurrent.Future  
import cats.syntax.traverse._
import cats.instances.future._
import cats.instances.list._
import scala.concurrent.ExecutionContext.Implicits.global
import cats.Id
import cats.Applicative
import cats.Functor

trait UptimeClient[F[_]] {
  def getUptime(hostname: String): F[Int]
}

class UptimeService[F[_]: Applicative](client: UptimeClient[F]) {
  def getTotalUptime(hostnames: List[String]): F[Int] = 
    Functor[F].fmap(hostnames.traverse(client.getUptime))(_.sum)
}

trait RealUptimeClient extends UptimeClient[Future] {
  def getUptime(hostname: String): Future[Int]
}

trait TestUptimeClient extends UptimeClient[Id] {
  def getUptime(hostname: String): Id[Int]  
}

class MyTestUptimeClient(hosts: Map[String, Int]) extends TestUptimeClient {
  def getUptime(hostname: String): Id[Int] = hosts.getOrElse(hostname, 0)  
}


object TestCode extends App {
  def testTotalUptime() = {
    val hosts = Map("host1" -> 10, "host2" -> 6)
    val client = new MyTestUptimeClient(hosts)
    val service = new UptimeService(client)
    val actual = service.getTotalUptime(hosts.keys.toList)
    val expected = hosts.values.sum
    println(actual == expected)    
  }
  testTotalUptime()
}