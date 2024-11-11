package part3datamanipulation

import cats.Semigroup

import scala.annotation.tailrec

object DataValidation {

  // encapsulates data validation logic and the errors
  // acts like an Either, has left and right type
  import cats.data.Validated

  val aValidValue: Validated[String, Int] = Validated.valid(42) // "Right" value
  val anInvalidValue: Validated[String, Int] = Validated.invalid("Something went wrong") // "Left" value
  val aTest: Validated[String, Int] = Validated.cond(42 > 39, 99, "meaning of life is too small")

  // the contract is different from Either, can combine the validation errors into one, using pure FP

  // Exercise 1: use Either to test the following conditions
  /*
    - n must be a prime
    - n must be non-negative
    - n <= 100
    - n must be even
   */
  // all the conditions that fail should be in the Left list
  def testNumber(n: Int): Either[List[String], Int] = {
    def isNonNegative = if n >= 0 then List() else List("Negative")
    def isLessOrEqualTo100 = if n <= 100 then List() else List("Greater than 100")
    def isEven = if n % 2 == 0 then List() else List("Odd")
    def isPrime = if n > 1 && (2 until n).forall(n % _ > 0) then List() else List("Non prime")

    val allValidations = isNonNegative ::: isLessOrEqualTo100 ::: isEven ::: isPrime

    Either.cond(allValidations.isEmpty, n, allValidations)
  }

  // Daniel's solution
  def testPrime(n: Int): Boolean = {
    @tailrec
    def tailrecPrime(d: Int): Boolean =
      if (d <= 1) true
      else n % d != 0 && tailrecPrime(d - 1)

    if (n == 0 || n == 1 || n == -1) false else tailrecPrime(Math.abs(n / 2))
  }

  def testNumberDaniel(n: Int): Either[List[String], Int] = {
    val isNotEven: List[String] = if n % 2 == 0 then List() else List("Number must be even")
    val isNegative: List[String] = if n >= 0 then List() else List("Number must be non-negative")
    val isTooBig: List[String] = if n <= 100 then List() else List("Number must be less than or equal to 100")
    val isNotPrime: List[String] = if testPrime(n) then List() else List("Number must be a prime")

    if n % 2 == 0 && n <= 0 && n <= 100 && testPrime(n) then Right(n)
    else Left(isNotEven ++ isNegative ++ isTooBig ++ isNotPrime)
  }

  // the above solution with Eithers is quite clunky. Validated offers a more elegant way to combine validation errors
  import cats.instances.list.given
  given Semigroup[Int] = Semigroup.instance[Int](Math.max)
  def validateNumber(n: Int): Validated[List[String], Int] =
    Validated
      .cond(n % 2 == 0, n, List("Number must be even"))
      // here the semigroups for the combination are needed. For list, a default one can be used
      // For the number I defined my own, because I don't want them to be summed up
      .combine(Validated.cond(n >= 0, n, List("Number must be non-negative")))
      .combine(Validated.cond(n <= 100, n, List("Number must be <= 100")))
      .combine(Validated.cond(testPrime(n), n, List("Number must be a prime")))

  def main(args: Array[String]): Unit = {
    println(testNumber(2))
    println(testNumber(102))
  }
}
