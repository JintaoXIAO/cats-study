package jt

import cats.effect.IOApp
import cats.effect.IO
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random
import cats.instances.list._
import cats.syntax.parallel._
import cats.instances.parallel._
import cats.Traverse
import utils._
import cats.Parallel
import cats.NonEmptyParallel

object IOTraversal extends IOApp.Simple {

  def heavyComputation(string: String): Future[Int] = Future {
    Thread.sleep(Random.nextInt(1000))  
    string.split(" ").length
  }

  def workLoad: List[String] = List("I quite like CE", "Scala is great", "looking forward to some awesome stuff")  
  
  def clunkyFutures(): Unit = {
    val futures: List[Future[Int]] = workLoad map heavyComputation
    futures.foreach(_.foreach(println))
  }  

  def traverseFutures(): Unit = {
    val singleFuture: Future[List[Int]] = Traverse[List].traverse(workLoad)(heavyComputation)  
    singleFuture.foreach(println)
  }  

  def computeAsIO(string: String): IO[Int] = IO {
    Thread.sleep(Random.nextInt(1000))
    string.split(" ").length
  }.myDebug  

  val ios: List[IO[Int]] = workLoad map computeAsIO
  val singleIO: IO[List[Int]] = Traverse[List].traverse(workLoad)(computeAsIO)  
  val parSingleIO: IO[List[Int]] = workLoad parTraverse computeAsIO
  val parSingleIO_v1: IO[List[Int]] = Parallel.parTraverse(workLoad)(computeAsIO)

  def sequence[A](listOfIOs: List[IO[A]]): IO[List[A]] = 
    Traverse[List].traverse(listOfIOs)(identity)

  def sequence_v2[F[_]: Traverse, A](listOfIOs: F[IO[A]]): IO[F[A]] = 
    Traverse[F].traverse(listOfIOs)(identity)  

  def parSequence[A](listOfIOs: List[IO[A]]): IO[List[A]] = 
    Parallel.parSequence(listOfIOs)

  def parSequence_v2[F[_]: Traverse, A](listOfIOs: F[IO[A]]): IO[F[A]] = 
    Parallel.parSequence(listOfIOs)  
  override def run: IO[Unit] = 
    //singleIO.map(_.sum).myDebug.void  
    parSingleIO.map(_.sum).myDebug.void  
}