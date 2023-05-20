package jt

import cats.effect.IO

package object utils {
  implicit  class DebugWrapper[A](io: IO[A]) {
    def myDebug: IO[A] = for {
      a <- io
      t = Thread.currentThread().getName
      id = Thread.currentThread().getId()
      _ <- IO.println(s"[$t($id)] $a")
    } yield a
  }
}