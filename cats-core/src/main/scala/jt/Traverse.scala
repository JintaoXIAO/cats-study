package jt

package traverse

object Example01 extends App {
  import scala.concurrent._
  import scala.concurrent.duration._
  import scala.concurrent.ExecutionContext.Implicits.global

  val hostnames = List(
    "alpha.example.com",
    "beta.example.com",
    "gama.demo.com"  
  )  

  def getUptime(hostname: String): Future[Int] = Future(hostname.length * 60)

  val allUptimes: Future[List[Int]] = 
    hostnames.foldLeft(Future(List.empty[Int])) {
      (acc, host) =>
        val uptime = getUptime(host)  
        for {
          acc <- acc
          uptime <- uptime
        } yield acc :+ uptime
    }  
  val r = Await.result(allUptimes, 1.second)
  println(r)

  val allUptimes1: Future[List[Int]] = Future.traverse(hostnames)(getUptime)
}

object Example03 extends App {
  import cats.Applicative
  import cats.instances.future._
  import cats.syntax.applicative._
  import cats.syntax.apply._
  import scala.concurrent.Future
  import scala.concurrent.ExecutionContext.Implicits.global
  
  def getUptime(hostname: String): Future[Int] = Future(hostname.length * 60)

  def oldCombine(acc: Future[List[Int]], host: String): Future[List[Int]] = {
    val uptime = getUptime(host)  
    for {
      acc <- acc
      uptime <- uptime  
    } yield acc :+ uptime
  }

  def newCombine(acc: Future[List[Int]], host: String): Future[List[Int]] =  
    (acc, getUptime(host)) mapN { _ :+ _ } 

  def listTraverse[F[_]: Applicative, A, B](list: List[A])(func: A => F[B]): F[List[B]] = 
    list.foldLeft(List.empty[B].pure[F]){ (acc, item) => 
        (acc, func(item)) mapN { _ :+ _ }  
      }      

  def listSequence[F[_]: Applicative, B](list: List[F[B]]): F[List[B]] =
    listTraverse(list)(identity)       
}

object Exercise extends App {
  import cats.instances.vector._
  import cats.data.Validated
  import cats.instances.list._
  import cats.instances.option._
  import cats.syntax.apply._
  val a = Example03.listSequence(List(Vector(1,2), (Vector(3,4))))  
  println(a)
  val b = Example03.listSequence(List(Vector(1,2)))
  println(b)  

  val c = Example03.listSequence(List(Vector(1, 2), Vector(3, 4), Vector(5, 6)))
  println(c)  

  def process(inputs: List[Int]) = 
    Example03.listTraverse(inputs)(n => if(n % 2 == 0) Some(n) else None)    
  val d = process(List(1,2,3))
  println(s"d: $d")

  val e = process(List(2,4,6))  
  println(e)

  type ErrorOr[A] = Validated[List[String], A]
  
  def process1(inputs: List[Int]): ErrorOr[List[Int]] = 
    Example03.listTraverse(inputs){ n => 
      if(n % 2 == 0) Validated.valid(n)
      else Validated.invalid(List(s"$n is not even"))
    }  

  println(process1(List(1,2,3)))   
  println(process1(List(2,4,6))) 
}