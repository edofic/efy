import annotation.implicitNotFound
import tlist._

package object tlist {
  sealed trait TList
  sealed trait ::[A, B <: TList] extends TList
  sealed trait TNil extends TList
}

@implicitNotFound( msg = "Cannot prove ${A} is in {B}" )
sealed trait In[A,B <: TList]

sealed trait InAux {
  implicit def tail[A,H,T <: TList](implicit in: In[A,T]): In[A, H :: T] = new In[A,H :: T]{}
}

object In extends InAux {
  implicit def head[A,T <: TList]: In[A, A :: T] = new In[A, A :: T]{}
}

@implicitNotFound( msg = "Cannot prove ${A} is not in {B}" )
sealed trait NotIn[A,B <: TList]

sealed trait NotInAux2 {
  implicit def nil[A]: NotIn[A, TNil] = new NotIn[A, TNil]{}
}

sealed trait NotInAux1 extends NotInAux2 {
  implicit def tail[A,H,T <: TList](implicit in: NotIn[A,T]): NotIn[A, H :: T] = new NotIn[A,H :: T]{}
}

object NotIn extends NotInAux1 {
  implicit def headAmb1[A,T <: TList]: NotIn[A, A :: T] = null
  implicit def headAmb2[A,T <: TList]: NotIn[A, A :: T] = null
}

sealed trait Union[A <: TList,B <: TList,O <: TList] {
  def a(deps: O): A = deps match {
    case x: A => x
    case _ => ???
  }
  def b(deps: O): B = deps match {
    case x: B => x
    case _ => ???
  }
}

trait UnionAux2 {
  implicit def cons[H, T <:TList, B <: TList, TT <: TList](implicit not: NotIn[H,B], u: Union[T,B,TT]): Union[H :: T, B, H :: TT] = 
    new Union[H :: T, B, H :: TT]{}
}

trait UnionAux1 extends UnionAux2 {
  implicit def consIn[H, T <: TList, B <: TList, TT <: TList](implicit in: In[H,B], u: Union[T,B,TT]): Union[H :: T, B, TT] =
    new Union[H :: T, B, TT]{}
}

object Union extends UnionAux1 {
  implicit def nil[A <: TNil,B <: TList]: Union[A,B,B] = new Union[A,B,B]{}
}

class Q[A <: TList, B <: TList]{
  def res[R <: TList](implicit u: Union[A,B,R]): () => R = () => ???
}


sealed trait Depends[A <: TList, R] {
  self =>

  def map[RR](f: R => RR): Depends[A, RR] = new Depends[A,RR]{
    def run(deps: A): RR = f(self run deps)
  }
  
  def flatMap[AA <: TList, RR, AAA <: TList](f: R => Depends[AA,RR])(implicit union: Union[A,AA,AAA]): Depends[AAA,RR] = new Depends[AAA,RR] {
    def run(deps: AAA): RR = {
      val temp = self run union.a(deps)
      f(temp) run union.b(deps)
    }
  }
    
  def run(deps: A): R
}

object Depends {
  def apply[A]: Depends[A :: TNil,A] = new Depends[A :: TNil,A]{
    def run(deps: A :: TNil): A = ???
  }
}
