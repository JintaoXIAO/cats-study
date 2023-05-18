package jt

package foldable

object Example01 extends App {
  val l = List("hello", "world", "from", "scala")  
  val fl = l.foldLeft(Nil: List[String])((b, a) => a :: b)
  val fr = l.foldRight(Nil: List[String])((a, b) => a :: b)
  println(fl)
  println(fr)
}

object Example02 extends App {
  import cats.Foldable  
  import cats.Eval
  import cats.instances.lazyList._

  def bigData = (1 to 100000).to(LazyList)

  val eval: Eval[Long] = 
    Foldable[LazyList].foldRight(bigData, Eval.now(0L)) { (num, eval) => 
        eval.map(_ + num)  
      }

  println(eval.value)      
}