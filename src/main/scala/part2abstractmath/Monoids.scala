package part2abstractmath

object Monoids {

//  import cats.Semigroup
  import cats.instances.int.given
  import cats.syntax.semigroup.given // import the |+| extension method

  val numbers = (1 to 1000).toList

  // |+| is always associative, doesn't matter in which order the values are combined
  val sumLeft = numbers.foldLeft(0)(_ |+| _)
  val sumRight = numbers.foldRight(0)(_ |+| _)

  // define a general API

  // Semigroup is not enough for this function because it doesn't tell us the starting (zero, neutral, empty) value
//  def combineFold[T](list: List[T])(using semigroup: Semigroup[T]): T =
//    list.foldLeft(/* What to put here? */)(_ |+| _)

  // MONOID - extension of Semigroup, provides a zero value
  import cats.Monoid
  val intMonoid = Monoid[Int]
  val combineInt = intMonoid.combine(23, 999) // same as Semigroup
  val zero = intMonoid.empty // intuitive/natural zero value for the type - in this case 0

  import cats.instances.string.given // brings an implicit Monoid[String] into the scope

  val emptyString = Monoid[String].empty // ""
  val combineString = Monoid[String].combine("U understand ", "monoids")

  import cats.instances.option.given // can now construct an implicit Monoid[Option[Int]]

  val emptyOption = Monoid[Option[Int]] // None
  val combineOptions = Monoid[Option[Int]].combine(Option(2), Option.empty[Int])
  val combineOptions2 = Monoid[Option[Int]].combine(Option(2), Option(6)) // Some(8)

  // extension methods for Monoids - |+| (same as Semigroup)
  // I don't even need to import cats.syntax.monoid because the method is imported via Semigroup which is a parent of Monoid

//  import cats.syntax.monoid.given

  val combinedOptionFancy = Option(3) |+| Option(7)

  // Exercise 1: implement combineFold
  def combineFold[T](list: List[T])(using monoid: Monoid[T]): T =
    list.foldLeft(monoid.empty)(_ |+| _)

  // Exercise 2: combine a list of phonebooks as maps of type Map[String, Int]
  // don't need to construct a monoid myself
  val phonebooks = List(
    Map(
      "Alice" -> 235,
      "Bob" -> 647
    ),
    Map(
      "Charlie" -> 372,
      "Daniel" -> 889
    ),
    Map(
      "Tina" -> 123
    )
  )

  import cats.instances.map.given
  val combinedPhonebook = combineFold(phonebooks)

  // Exercise 3: shopping cart and online stores with Monoids
  // hint: define your own monoid - Monoid.instance
  // hint 2: use combineFold
  case class ShoppingCart(items: List[String], total: Double)

  given Monoid[ShoppingCart] = Monoid.instance(
    ShoppingCart(List.empty, 0.0),
    (cart1, cart2) => ShoppingCart(cart1.items ::: cart2.items, cart1.total + cart2.total)
  )

  def checkout(shoppingCarts: List[ShoppingCart]): ShoppingCart = combineFold(shoppingCarts)

  def main(args: Array[String]): Unit = {
    println(sumLeft)
    println(sumRight)
    println(combineOptions)
    println(combineFold(numbers))
    println(combineFold(List("One ", "Two ", "Three")))
    println(combinedPhonebook)
    println(
      checkout(
        List(
          ShoppingCart(List("headphones", "mouse"), 55.5),
          ShoppingCart(List("chair", "sofa"), 550.9),
          ShoppingCart(List(), 0.0)
        )
      )
    )
  }
}
