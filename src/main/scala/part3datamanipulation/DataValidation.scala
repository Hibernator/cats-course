package part3datamanipulation

import cats.Semigroup

import scala.annotation.tailrec
import scala.util.Try

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

  // chaining Validated
  // andThen function - works like flatMap but doesn't short-circuit in case the original value is invalid
  // that's why it's not called flatMap
  aValidValue.andThen(_ => anInvalidValue) // if the aValidValue was invalid, nothing would happen

  // test a validated value, with an invalid fallback in case the test fails
  aValidValue.ensure(List("Something went wrong"))(_ % 2 == 0)

  // transform
  aValidValue.map(_ + 1)
  aValidValue.leftMap(_.length)
  aValidValue.bimap(_.length, _ + 1)

  // interop with Scala stdlib - mainly with Either, Option, Try
  val eitherToValidated: Validated[List[String], Int] = Validated.fromEither(Right(42))
  val optionToValidated: Validated[List[String], Int] = Validated.fromOption(Some(42), List("Nothing is present here"))
  val tryToValidated: Validated[Throwable, Int] = Validated.fromTry(Try("something".toInt))

  // backwards API
  aValidValue.toOption
  aValidValue.toEither
  // no method to transform to Try because the error type is missing

  // Exercise 2 - form validation
  object FormValidation {
    type FormValidation[T] = Validated[List[String], T]

    def getValue(form: Map[String, String], fieldName: String): FormValidation[String] =
      Validated.fromOption(form.get(fieldName), List(s"The field $fieldName must be specified"))

    // Daniel's validation functions
    def nonBlank(value: String, fieldName: String): FormValidation[String] =
      Validated.cond(value.length > 0, value, List(s"The field $fieldName must be not blank"))

    def emailProperForm(email: String): FormValidation[String] =
      Validated.cond(email.contains("@"), email, List("Email is invalid"))

    def passwordCheck(password: String): FormValidation[String] =
      Validated.cond(password.length >= 10, password, List("Password must be at least 10 characters long"))

    /*
      Fields are:
      - name
      - email
      - password

      Rules are:
      - name, email and password must be specified
      - name must not be blank
      - email must have "@"
      - password must have >= 10 characters

      If the validation is successful, return Valid("Success")
     */
    def validateForm(form: Map[String, String]): FormValidation[String] =
      val nameValidated =
        getValue(form, "name").ensure(List("Name is blank"))(!_.isBlank)
      val emailValidated = getValue(form, "email")
        .ensure(List("Email doesn't have @"))(_.contains("@"))
      val passwordValidated = getValue(form, "password")
        .ensure(List("Password has less than 10 characters"))(_.length >= 10)

      nameValidated.combine(emailValidated).combine(passwordValidated).map(_ => "Success")

    def validateFormDaniel(form: Map[String, String]): FormValidation[String] =
      getValue(form, "name")
        .andThen(nonBlank(_, "name"))
        // Semigroup of String is needed too, for combining the valid values
        .combine(getValue(form, "email").andThen(emailProperForm))
        .combine(getValue(form, "password").andThen(passwordCheck))
        .map(_ => "Success")
  }

  val validForm = Map(
    "name" -> "Bolton",
    "email" -> "bolton@dreadfort.com",
    "password" -> "reallystrongpassword"
  )

  val invalidForm = Map(
    "email" -> "boltonAtdreadfort.com",
    "password" -> "short"
  )

  val validFormValidation = FormValidation.validateForm(validForm)
  val invalidFormValidation = FormValidation.validateForm(invalidForm)

  // Nice API
  import cats.syntax.validated.*

  val aValidMeaningOfLife: Validated[List[String], Int] = 42.valid[List[String]]
  val anError: Validated[String, Int] = "Something is invalid".invalid[Int]

  def main(args: Array[String]): Unit = {
    println(testNumber(2))
    println(testNumber(102))
    println(DataValidation.validFormValidation)
    println(DataValidation.invalidFormValidation)
  }
}
