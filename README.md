#efy

monadic approach to effect system and dependency injection in scala

This is a very much work in progress!

The main idea is to request a dependency out of thin air. But rather than chaining it throughout the stack via implicits you return an object that is "a continuation" when the dependency is injected. 
Utilizing the turing-complete type system of scala you can infer types of dependencies to inject and you have to write almost zero boilerplate(except where you need explicit type annotations - e.g. recursion)

### The Catch
Of course there is a catch. Your code must be written in monadic style because the dependency is wrapped in a monad. I plan on embedding some monad transformers in the near future to make working with futures, options and some other types pleasant - in other words: no nested maps and flatMaps will be necessary. 

### Sample
```scala
object Sample extends App {
  val res = for{ 
    a <- Depends[Int]
    b <- Depends[String] 
  } yield a.toString + b
  
  val deps = Dependencies.empty + 1 + "st"
  println(res run deps)
}
```

Note that everything is typechecked. At compilation time `res` contains the information that it needs a `String` and an `Int` value. And `deps` also contains types of all it's members. `res run deps` will not compile if `deps` types are not a subset of `res` dependencies(order doesn't matter!).
