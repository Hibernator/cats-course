package part1intro

object CatsIntro {

  // Eq type class - allows to compare values at compile time. Fails compilation if values are of different types
  // Probably not as useful in Scala 3 because of multiversal/strict equality feature

  // the comparison expression doesn't compile in Scala 3 because of primitive types
  // Would compile otherwise unless strict/multiversal equality is enabled
  // I need a CanEqual[Int, String] given instance available, can be derived automatically
  // https://www.baeldung.com/scala/multiversal-equality
  given CanEqual[Int, String] = CanEqual.derived
  val aComparison: Boolean = 2 == "a string"

  // part 1 - type class import
  import cats.Eq

  // part 2 - import type class instances for the types you need
  // in this case type class instances for Int are imported
//  import cats.instances.int._
  import cats.instances.int.given

  // In case I don't know what to import, import all type classes, their instances and extension methods
  // import cats._
  // import cats.implicits._

  // part 3 - use type class API
  val intEquality = Eq[Int]
  val aTypeSafeComparison = intEquality.eqv(2, 3) // false
//  val anUnsafeComparison = intEquality.eqv(2, "a string") // doesn't compile

  // part 4 - use extension methods (if applicable)
  // all extension methods for Eq type class are imported
  // extension methods are only visible in the presence of the right type class instance
  // (wouldn't work on Strings right now)
//  import cats.syntax.eq._
  import cats.syntax.eq.given
  val anotherTypeSafeComparison = 2 === 3 // false
  val neqComparison = 2 =!= 3 // true
//  val invalidComparison = 2 === "a string" // doesn't compile

  // part 5 - extending type class operations to composite classes like collections
  import cats.instances.list.given // Eq[List[Int]] in scope now (and extension method is now available as well)
  val aListComparison = List(2) === List(3) // false

  // part 6 - what if my type is not supported? Create a type class instance for a custom type
  // there is a special method for that
  case class ToyCar(model: String, price: Double)
  given Eq[ToyCar] = Eq.instance[ToyCar] { (car1, car2) =>
    car1.price == car2.price
  }

  val compareTwoToyCars = ToyCar("Ferrari", 29.99) === ToyCar("Lamborghini", 29.99) // true
}
