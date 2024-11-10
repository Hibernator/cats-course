package part3datamanipulation

import cats.Eval
import cats.data.IndexedStateT

object FunctionalState {

  // In cats state is a data structure that describes evolution of a system

  // S is the type of a state
  // A is the answer, desirable value obtained after a single computation
  type MyState[S, A] = S => (S, A)

  import cats.data.State

  // State is a wrapper of a function, in cats it's actually an Eval
  val countAndSay: State[Int, String] = State(currentCount => (currentCount + 1, s"Counted $currentCount"))
  // This is how a function is called, just more complicated
  val (eleven, counted10) = countAndSay.run(10).value

  // States can be composed, which allows expressing iterative computations in a purely functional way without mutations
  // State is abstraction for "iterative" computations

  // here's a really bad Scala code with vars and mutations
  var a = 10
  a += 1
  val firstComputation = s"Added 1 to 10, obtained $a"
  a *= 5
  val secondComputation = s"Multiplied with 5, obtained $a"

  // rewriting the above code in a purely FP way with states
  val firstTransformation: State[Int, String] = State((s: Int) => (s + 1, s"Added 1 to 10, obtained ${s + 1}"))
  val secondTransformation: State[Int, String] = State((s: Int) => (s * 5, s"Multiplied with 5, obtained ${s * 5}"))
  // now we compose the two above states into one and run for an initial value
  // the state calculation are done automatically, we only need to handle the answers
  val compositeTransformation: State[Int, (String, String)] = firstTransformation.flatMap { firstResult =>
    secondTransformation.map(secondResult => (firstResult, secondResult))
  }
  val compositeTransformationFor: State[Int, (String, String)] = for {
    firstResult <- firstTransformation
    secondResult <- secondTransformation
  } yield (firstResult, secondResult)

  // Let's try to express the same with just two functions (instead of States) and chain them
  val func1: Int => (Int, String) = (s: Int) => (s + 1, s"Added 1 to 10, obtained ${s + 1}")
  val func2: Int => (Int, String) = (s: Int) => (s * 5, s"Multiplied with 5, obtained ${s * 5}")
  // the final state is nested inside the inner tuple
  // in order to compose further we would need to decompose nested tuples
  // the more computation chained, the more nested tuples to decompose
  val compositeFunc: Int => (String, (Int, String)) = func1.andThen { (newState, firstResult) =>
    (firstResult, func2(newState))
  }

  // Exercise: an online store
  case class ShoppingCart(items: List[String], total: Double)
  // transformation from old shopping cart to a new shopping cart and total amount at that point
  def addToCart(item: String, price: Double): State[ShoppingCart, Double] =
    State(cart => (cart.copy(items = item :: cart.items, total = cart.total + price), cart.total + price))

  val danielsCart: State[ShoppingCart, Double] = for {
    _ <- addToCart("Fender guitar", 500)
    _ <- addToCart("Elixir strings", 19)
    total <- addToCart("Electric cable", 8)
  } yield total

  // Exercise 2: pure mental gymnastics, implement the 4 functions below
  // returns a State data structure that, when run, will not change the state but will issue the value f(a)
  def inspect[A, B](f: A => B): State[A, B] = State(value => (value, f(value)))
  // returns a State data structure that, when run, returns the value of that state and makes no changes
  def get[A]: State[A, A] = State(value => (value, value))
  // returns a State data structure that, when run, returns Unit and sets the state to that value
  def set[A](value: A): State[A, Unit] = State(_ => (value, ()))
  // returns a State data structure that, when run, returns Unit and sets the state to f(state)
  def modify[A](f: A => A): State[A, Unit] = State(value => (f(value), ()))

  // all 4 above methods are already implemented in the State companion object
  import cats.data.State._

  // this is basically a sequential program written with the immutable values of State and map and flatMap methods
  // reduction of imperative program to a for-comprehension
  // imperative program reduced to a pure functional program
  val program: State[Int, (Int, Int, Int)] = for {
    a <- get[Int]
    _ <- set[Int](a + 10)
    b <- get[Int]
    _ <- modify[Int](_ + 43)
    c <- inspect[Int, Int](_ * 2)
  } yield (a, b, c)

  def main(args: Array[String]): Unit = {
    val stateResult: (Int, (String, String)) = compositeTransformationFor.run(10).value
    println(stateResult)

    // the final state is nested
    val funcResult = compositeFunc(10)
    println(funcResult)

    println(danielsCart.run(ShoppingCart(List(), 0)).value)

    // trying the exercise 2 methods
    println(inspect((in: Int) => s"StrValue: ${in.toString}").run(1).value)
    println(get.run(1).value)
    println(set(1).run(2).value)
    println(modify((in: Int) => in + 1).run(1).value)
  }
}
