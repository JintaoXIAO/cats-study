package jt

import scala.concurrent.duration._
import cats.effect.IOApp
import cats.effect.IO
import utils._
import scala.concurrent.ExecutionContext
import java.util.concurrent.Executors

object BlockingIOs extends IOApp.Simple {

  val someSleeps = for {
    _ <- IO.sleep(1.second).myDebug
    _ <- IO.sleep(1.second).myDebug
  } yield ()

  val aBlockingIO = IO.blocking {
    Thread.sleep(1000)  
    println(s"[${Thread.currentThread.getName}] computed a blocking code")
  }

  val iosOnManyThreads = for {
    _ <- IO("first").myDebug
    _ <- IO.cede
    _ <- IO("second").myDebug
    _ <- IO.cede
    _ <- IO("third").myDebug  
  } yield ()

  def test() = {
    val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))  
    (1 to 100).map(IO.pure).reduce(_.myDebug >> IO.cede >> _.myDebug).evalOn(ec)
  }
  override def run: IO[Unit] = 
    // someSleeps  
    // aBlockingIO
    test().void
}