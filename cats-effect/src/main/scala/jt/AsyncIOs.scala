package jt

import cats.syntax.either._
import cats.syntax.option._
import cats.effect.IOApp
import cats.effect.IO
import utils._
import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.util.Try
import scala.concurrent.Future
import scala.concurrent.Await
import scala.util.Success
import scala.util.Failure


object AsyncIOs extends IOApp.Simple {

  val threadPool = Executors.newFixedThreadPool(8)
  implicit val ec = ExecutionContext.fromExecutorService(threadPool)

  type Callback[A] = Either[Throwable, A] => Unit

  def computeMeaningOfLifeEither(): Either[Throwable, Int] = Try {
    Thread.sleep(1000)
    println(s"[${Thread.currentThread.getName}] computing the meaning of life on some other thread...")
    32
  }.toEither

  def computeMeaningOfLife(): Int = {
    Thread.sleep(1000)
    println(s"[${Thread.currentThread.getName}] computing the meaning of life on some other thread...")
    32
  }

  def computeMolOnThreadPool(): Unit = {
    threadPool.execute(() => computeMeaningOfLifeEither())  
  }  

  def asyncMolIO: IO[Int] = IO.async_ { cb => 
    threadPool.execute { () => 
      val r = computeMeaningOfLifeEither() 
      cb(r)
    }
  } 

  def asyncToIO[A](computation: () => A)(ec: ExecutionContext): IO[A] = IO.async_{ cb => 
    ec.execute{ () => 
      cb(Try { computation() }.toEither)  
    }
  }

  def asyncMolIO_v2 = asyncToIO(computeMeaningOfLifeEither)(ec)  

  lazy val molFuture = Future { computeMeaningOfLife() }(ec)    

  def convertFutureToIO[A](future: => Future[A])(implicit ec: ExecutionContext): IO[A] = IO.async_ { cb =>
    future.onComplete(r => cb(r.toEither))
  }  

  val asyncMolIO_v3: IO[Int] = convertFutureToIO(molFuture)  
  
  val neverEndingIO: IO[Int] = IO.async_[Int]{ _ => () }

  val asyncMeaningOfLife_v2: IO[Int] = IO.async { cb => 
    IO {
      threadPool.execute { () => 
        val r = computeMeaningOfLifeEither()
        cb(r)  
      }  
    }.as(IO("Cancelled!").myDebug.void.some)  
  }

  def demoAsyncCancellation() = {
    for {
      fib <- asyncMeaningOfLife_v2.start
      _ <- IO.sleep(500.milliseconds) >> IO("cancelling...").myDebug >> fib.cancel
      _ <- fib.join
    } yield ()
  }  

  override def run: IO[Unit] = 
    // asyncMolIO.myDebug >> IO(threadPool.shutdown())
    // asyncMolIO_v2.myDebug >> IO(threadPool.shutdown())
    // asyncMolIO_v3.myDebug >> IO(threadPool.shutdown())
    demoAsyncCancellation() >> IO(threadPool.shutdown())
}