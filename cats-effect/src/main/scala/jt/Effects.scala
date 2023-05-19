package jt

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.effect.IOApp
import scala.io.StdIn

object Effects extends IOApp.Simple {

  def now: IO[Long] = IO { val a = System.currentTimeMillis(); println(a); a }

  def measure[A](computation: IO[A]): IO[Long] = for {
    start <- now  
    _ <- computation
    end <- now
  } yield end - start

  def get: IO[String] = IO { StdIn.readLine() }  
  def put(str: String): IO[Unit] = IO { println(str) }

  def run: IO[Unit] = {
    measure(IO { Thread.sleep(2); 10 }).flatMap(duration => IO.println(s"time used: $duration"))
  }
  
}