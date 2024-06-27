package part2abstractmath

object Semigroups {

  // Semigroups COMBINE elements of the same type
  import cats.Semigroup
  import cats.instances.int.given

  val naturalIntSemigroup = Semigroup[Int]
  val intCombination = naturalIntSemigroup.combine(2, 46) // addition

  import cats.instances.string.given
  val naturalStringSemigroup = Semigroup[String]
  val stringCombination = naturalStringSemigroup.combine("I love ", "Cats") // concatenation

  // specific reduction API, I could just provide a combination function manually, without semigroups
  def reduceInts(list: List[Int]): Int = list.reduce(naturalIntSemigroup.combine)
  def reduceStrings(list: List[String]): String = list.reduce(naturalStringSemigroup.combine)

  // general reduction API, I just need a semigroup TC given instance (from imports)
  def reduceThings[T](list: List[T])(using semigroup: Semigroup[T]): T = list.reduce(semigroup.combine)

  // Exercise 1: support a new type
  // hint: use the same pattern we used with Eq (Eq.instance)
  case class Expense(id: Long, amount: Double)
  given expenseSemigroup: Semigroup[Expense] = Semigroup.instance { (expense1, expense2) =>
    Expense(expense1.id.max(expense2.id), expense1.amount + expense2.amount)
  }

  // Extension methods from Semigroup - |+| (combine function)
  import cats.syntax.semigroup.given
  val anIntSum = 2 |+| 3 // "two combine three", requires a presence of given Semigroup[Int]
  val aStringConcat = "we like " |+| "semigroups"
  val aCombinedExpense = Expense(4, 80) |+| Expense(56, 46)

  // Exercise 2: implement reduceThings2 using the |+| function
  def reduceThings2[T: Semigroup](list: List[T]): T = list.reduce(_ |+| _)

  def main(args: Array[String]): Unit = {
    println(intCombination)
    println(stringCombination)

    // using the specific API
    val numbers = (1 to 10).toList
    println(reduceInts(numbers)) // compiler injects the given Semigroup[Int]
    val strings = List("I'm ", "starting ", "to ", "like ", "semigroups")
    println(reduceStrings(strings)) // compiler injects the given Semigroup[String]

    // using general API
    println(reduceThings(numbers))
    println(reduceThings(strings))

    import cats.instances.option.given
    // compiler will produce a given Semigroup[Option[Int]] - combine will produce an option with summed elements
    // compiler will produce a given Semigroup[Option[String]] - combine will product an option with concatenated string
    val numberOptions: List[Option[Int]] = numbers.map(n => Option(n))
    println(reduceThings(numberOptions)) // Option[Int] containing the sum of all the numbers
    val stringsOptions: List[Option[String]] = strings.map(s => Option(s))
    println(reduceThings(stringsOptions))

    // Test exercise 1
    val expenses = List(Expense(1, 1.2), Expense(3, 3.4), Expense(2, 5.6))
    println(reduceThings(expenses))

    // Test exercise 2
    println(reduceThings2(expenses))
  }
}
