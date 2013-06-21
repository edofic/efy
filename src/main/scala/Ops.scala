import annotation.implicitNotFound
import tlist._

package object tlist {
  sealed trait TList
  sealed trait ::[A, B <: TList] extends TList
  sealed trait TNil extends TList
}


sealed trait Union[A,B,O] {
  def a(deps: Dependencies[O]): Dependencies[A] = deps match {
    case x: Dependencies[A] => x
    case _ => ???
  }
  def b(deps: Dependencies[O]): Dependencies[B] = deps match {
    case x: Dependencies[B] => x
    case _ => ???
  }
}

object Union {
  implicit def wth[A,B]: Union[A,B,A with B] = new Union[A,B,A with B]{}
}

case class Dependencies[+A](a: A)

sealed trait Depends[A, R] {
  self =>

  def map[RR](f: R => RR): Depends[A, RR] = new Depends[A,RR]{
    def run(deps: Dependencies[A]): RR = f(self run deps)
  }
  
  def flatMap[AA, RR, AAA](f: R => Depends[AA,RR])(implicit union: Union[A,AA,AAA]): Depends[AAA,RR] = new Depends[AAA,RR] {
    def run(deps: Dependencies[AAA]): RR = {
      val temp = self run union.a(deps)
      f(temp) run union.b(deps)
    }
  }
    
  def run(deps: Dependencies[A]): R
}

object Depends {
  def apply[A]: Depends[A,A] = new Depends[A,A]{
    def run(deps: Dependencies[A]): A = deps.a
  }
}

object Sample extends App {
  trait I{def n:Int}
  trait S{def s: String}
  val res = for{ a <- Depends[I]; b <- Depends[S] } yield a.n.toString + b.s
  val deps = Dependencies( new I with S{ val n=1; val s = "st" } )
  println(res run deps)
}
