package jt

import cats.kernel.Monoid
import scala.concurrent.{ Future, Await }
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import cats.instances.vector._
import cats.instances.list._
import cats.instances.future._
import cats.syntax.traverse._
import cats.syntax.foldable._
import cats.syntax.semigroup._
import cats.Traverse
import cats.Foldable

object MapReduce extends App {
  def foldMap[A, B](vec: Vector[A])(f: A => B)(implicit mb: Monoid[B]): B = 
    (vec map f).foldLeft(mb.empty)(mb.combine)  

  val a = foldMap(Vector(1,2,3))(identity); println(a)    
  val b = foldMap(Vector(1,2,3))(_.toString + "! "); println(b)
  val c = foldMap("Hello world!".toVector)(_.toString.toUpperCase);println(c)
}

object MapReduceParallel extends App {
  def parallelFoldMap[A, B: Monoid](values: Vector[A])(func: A => B): Future[B] = {
    val mb = implicitly[Monoid[B]]    
    
    val cpus = Runtime.getRuntime.availableProcessors
    val groupSize = (1.0 * values.size / cpus).ceil.toInt    
    
    val groups: Iterator[Vector[A]] = values.grouped(groupSize)

    val futures: Iterator[Future[B]] = groups map { group => 
      Future { group.foldLeft(mb.empty){ _ |+| func(_) }}
    }

    Future.sequence(futures) map { it => 
      it.foldLeft(mb.empty)(mb.combine)  
    }
  }

  def parallelFoldMap1[A, B: Monoid](values: Vector[A])(func: A => B): Future[B] = {
    val mb = implicitly[Monoid[B]]    
    
    val cpus = Runtime.getRuntime.availableProcessors
    val groupSize = (1.0 * values.size / cpus).ceil.toInt    
    
    values
      .grouped(groupSize)
      .toVector
      .traverse{ group => Future { group.toVector.foldMap(func) } }
      .map(_.combineAll)
  }

  val result: Future[Int] = parallelFoldMap1((1 to 10000).toVector)(identity)  
  val r = Await.result(result, 1.seconds)
  println(r)
}