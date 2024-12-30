package part5alien

object Kleislis:

  // Generic data structure that helps composing functions returning wrapper instances

  val func1: Int => Option[String] = x => if x % 2 == 0 then Some(s"$x is even") else None
  val func2: Int => Option[Int] = x => Some(x * 3)

  // We want to compose the above functions like this
  // func3 = func2 andThen func1

  // would work for plain functions
  val plainFunc1: Int => String = x => if x % 2 == 0 then s"$x is even" else "fail"
  val plainFunc2: Int => Int = x => x * 3
  val plainFunc3: Int => String = plainFunc2 andThen plainFunc1

  // wrapper over a function that returns a wrapped value
  import cats.data.Kleisli
  val func1K: Kleisli[Option, Int, String] = Kleisli(func1)
  val func2K: Kleisli[Option, Int, Int] = Kleisli(func2)

  import cats.instances.option.given // FlatMap[Option] needed in scope
  val func3K: Kleisli[Option, Int, String] = func2K andThen func1K

  // convenience APIs

  // composition methods
  val multiply: Kleisli[Option, Int, Int] = func2K.map(_ * 2) // x => Option(...).map(_ * 2). Functor[Option] in scope
  val chain: Kleisli[Option, Int, Int] = func2K.flatMap(x => func2K) // FlatMap[Option] in scope
  // a lot of other convenience methods for composition available

  // Exercise - is there a relationship between InterestingKleisli and some other cats data structure
  // It is dependency injection - Reader in cats. Kleisli[Id, Int, Int] == Reader[Int, Int]
  import cats.Id
  type InterestingKleisli[A, B] = Kleisli[Id, A, B] // wrapper over A => Id[B]
  // InterestingKleisli == Reader
  // hint
  val times2: Kleisli[Id, Int, Int] = Kleisli[Id, Int, Int](x => x * 2)
  val plus4: Kleisli[Id, Int, Int] = Kleisli[Id, Int, Int](y => y + 4)
  val composed: Kleisli[Id, Int, Int] = times2.flatMap(t2 => plus4.map(p4 => t2 + p4))
  // when this is used, the argument goes to both Kleislis at the same time, they will be executed in paraller!!!
  // the result is combined
  val composedFor: Kleisli[Id, Int, Int] = for
    t2 <- times2
    p4 <- plus4
  yield t2 + p4

  // Summary
  // Kleisli is a wrapper over functions returning higher-kinded F[_] instances
  // Useful for composing such functions, has a lot of convenience functions

  def main(args: Array[String]): Unit = {
    println(composedFor(3)) // sum of 3*2 and 3+4
  }
