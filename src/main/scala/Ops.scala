import language.implicitConversions
import annotation.implicitNotFound
import reflect.ClassTag
import tlist._

package object tlist {
  sealed trait TList
  sealed trait ::[A, B <: TList] extends TList
  sealed trait TNil extends TList
}

@implicitNotFound( msg = "Cannot prove ${A} is in ${B}" )
sealed trait In[A,B <: TList]

sealed trait InAux {
  implicit def tail[A,H,T <: TList](implicit in: In[A,T]): In[A, H :: T] = null
}

object In extends InAux {
  implicit def head[A,T <: TList]: In[A, A :: T] = null
}

@implicitNotFound( msg = "Cannot prove ${A} is a subset of ${B}" )
sealed trait Subset[A <: TList, B <: TList]

sealed trait SubsetAux {
  implicit def ht[H, T <: TList, A <: TList](implicit h: In[H,A], t: Subset[T,A]): Subset[H :: T, A] = null
}

object Subset extends SubsetAux {
  implicit def nil[A <: TList]: Subset[TNil, A] = null
}

@implicitNotFound( msg = "Cannot prove ${A} is not in ${B}" )
sealed trait NotIn[A,B <: TList]

sealed trait NotInAux2 {
  implicit def nil[A]: NotIn[A, TNil] = null
}

sealed trait NotInAux1 extends NotInAux2 {
  implicit def tail[A,H,T <: TList](implicit in: NotIn[A,T]): NotIn[A, H :: T] = null
}

object NotIn extends NotInAux1 {
  implicit def headAmb1[A,T <: TList]: NotIn[A, A :: T] = null
  implicit def headAmb2[A,T <: TList]: NotIn[A, A :: T] = null
}

sealed trait Union[A <: TList,B <: TList,O <: TList] 

trait UnionAux2 {
  implicit def cons[H, T <:TList, B <: TList, TT <: TList](implicit not: NotIn[H,B], u: Union[T,B,TT]): Union[H :: T, B, H :: TT] = null
}

trait UnionAux1 extends UnionAux2 {
  implicit def consIn[H, T <: TList, B <: TList, TT <: TList](implicit in: In[H,B], u: Union[T,B,TT]): Union[H :: T, B, TT] = null
}

object Union extends UnionAux1 {
  implicit def nil[A <: TNil,B <: TList]: Union[A,B,B] = null
  
  object toSubset {
    implicit def left[A <: TList,B <: TList,O <: TList](implicit u: Union[A,B,O]): Subset[A,O] = null
    implicit def right[A <: TList,B <: TList,O <: TList](implicit u: Union[A,B,O]): Subset[B,O] = null
  }
}

class Dependencies[T <: TList] private[Dependencies] (map: Map[ClassTag[_], Any]) {
  def +[A](value: A)(implicit key: ClassTag[A]) = new Dependencies[A :: T](map + ((key, value)))
  
  def get[A](implicit in: In[A, T], key: ClassTag[A]) = map(key).asInstanceOf[A]
}

object Dependencies {
  implicit def tlistView[A <: TList, B <: TList](deps: Dependencies[A])(implicit sub: Subset[B,A]): Dependencies[B] = deps.asInstanceOf[Dependencies[B]]
  
  val empty = new Dependencies[TNil](Map.empty)
}

sealed trait Depends[A <: TList, R] {
  self =>

  def map[RR](f: R => RR): Depends[A, RR] = new Depends[A,RR]{
    def run(deps: Dependencies[A]): RR = f(self run deps)
  }
  
  def flatMap[AA <: TList, RR, AAA <: TList](f: R => Depends[AA,RR])(implicit union: Union[A,AA,AAA]): Depends[AAA,RR] = new Depends[AAA,RR] {
    import Union.toSubset._
  
    def run(deps: Dependencies[AAA]): RR = {
      val temp = self run deps
      f(temp) run deps
    }
  }
    
  def run(deps: Dependencies[A]): R
}

object Depends {
  def apply[A: ClassTag]: Depends[A :: TNil,A] = new Depends[A :: TNil,A]{
    def run(deps: Dependencies[A :: TNil]): A = deps.get[A]
  }
}
