import tlist._

object Sample extends App {
  val res = for{ 
    a <- Depends[Int]
    b <- Depends[String] 
  } yield a.toString + b
  
  val deps = Dependencies.empty + 1 + "st"
  println(res run deps)
}
