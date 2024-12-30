package part4typeclasses

import cats.{Applicative, Foldable, Functor, Monad}

import java.time.Duration
import java.util.concurrent.{ExecutorService, Executors}
import scala.concurrent.{ExecutionContext, Future}

object Traversing:
  // Higher-level approach to iteration

  val executorService: ExecutorService = Executors.newFixedThreadPool(8)
  given ExecutionContext = ExecutionContext.fromExecutorService(executorService)

  // pinging servers example
  val servers: List[String] = List("server-ci.rockthejvm.com", "server-staging.rockthejvm.com", "prod.rockthejvm.com")
  def getBandwidth(hostname: String): Future[Int] = Future(hostname.length * 80)

  // I want to gather all bandwidths of all servers in one future
  /*
      The problem is:
      we have
       - a List[String]
       - func: String => Future[Int]
      we want Future[List[Int]]
   */
  // very manual solution
  val allBandwidths: Future[List[Int]] = servers.foldLeft(Future(List.empty[Int])) { (accumulator, hostname) =>
    val bandFuture = getBandwidth(hostname)
    for
      accBandwidths <- accumulator
      band <- bandFuture
    yield accBandwidths :+ band
  }

  // More elegant
  // Inside, the traverse method also uses foldLeft
  val allBandwidthsTraverse: Future[List[Int]] = Future.traverse(servers)(getBandwidth)

  val allBandwidthsSequence: Future[List[Int]] = Future.sequence(servers.map(getBandwidth))

  // Exercise - implement the method
  // Similar to the traverse above but this time it's a general F container, not a Future
  import cats.syntax.applicative.given // pure
  import cats.syntax.flatMap.given // flatMap
  import cats.syntax.functor.given // map
  def listTraverseMonad[F[_]: Monad, A, B](list: List[A])(func: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure[F]) { (accWrapped, item) =>
      val transformedItemWrapped: F[B] = func(item)
      for
        acc <- accWrapped
        transformedItem <- transformedItemWrapped
      yield acc :+ transformedItem
    }

  // In fact, the minimum requirement for the method to work is a lower bound: Applicative instead of Monad
  // This allows usage of the method on wider array of structures, for example Validated, which is not monadic
  // Now, map and flatMap are not available
  // I can use the mapN method of Apply (weaker Applicative) instead
  import cats.syntax.apply.given // mapN
  def listTraverse[F[_]: Applicative, A, B](list: List[A])(func: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure[F]) { (accWrapped, item) =>
      val transformedItemWrapped: F[B] = func(item)
      (accWrapped, transformedItemWrapped).mapN(_ :+ _)
    }

  // Exercise - implement this method in terms of listTraverse
  def listSequence[F[_]: Applicative, A](list: List[F[A]]): F[List[A]] =
    listTraverse(list)(identity)

  // Exercise 3 - what will the following expression return
  import cats.instances.vector.given
  val allPairs: Vector[List[Int]] = listSequence(
    List(Vector(1, 2), Vector(3, 4))
  ) // Vector[List[Int]] - all possible 2-tuples in form of list, cartesian product
  val allTriples: Vector[List[Int]] = listSequence(
    List(Vector(1, 2), Vector(3, 4), Vector(5, 6))
  ) // Vector[List[Int]] - all possible 3-tuples in lists

  // Exercise 4: what's the result of the following expressions
  import cats.instances.option.given
  // this is basically like forall method
  def filterAsOption(list: List[Int])(predicate: Int => Boolean): Option[List[Int]] =
    listTraverse[Option, Int, Int](list)(n => Option.when(predicate(n))(n))

  val allTrue: Option[List[Int]] = filterAsOption(List(2, 4, 6))(_ % 2 == 0) // Some(List(2, 4, 6))
  val someFalse: Option[List[Int]] =
    filterAsOption(List(1, 2, 3))(_ % 2 == 0) // None - because Some combined with None yields None

  // listTraverse is also useful for non-Monad Applicatives
  import cats.data.Validated
  import cats.instances.list.given // Semigroup[List], enables Applicative[ErrorsOr]
  type ErrorsOr[T] = Validated[List[String], T]
  def filterAsValidated(list: List[Int])(predicate: Int => Boolean): ErrorsOr[List[Int]] =
    listTraverse[ErrorsOr, Int, Int](list) { n =>
      if predicate(n) then Validated.valid(n) else Validated.invalid(List(s"predicate for $n failed"))
    }

  // Exercise 5: what's the result of the following expressions
  val allTrueValidated: ErrorsOr[List[Int]] = filterAsValidated(List(2, 4, 6))(_ % 2 == 0) // Valid(List(2, 4, 6))
  // Invalid(List("predicate for 1 failed", "predicate for 3 failed"))
  val someFalseValidated: ErrorsOr[List[Int]] = filterAsValidated(List(1, 2, 3))(_ % 2 == 0)

  // We can generalize traversal to any kind of container

  /** @tparam L
    *   container of data
    */
  trait MyTraverse[L[_]] extends Foldable[L] with Functor[L]:
    // fundamental method which is abstract.
    // Above it's implemented with foldLeft which presumes existence of empty container, which is not always the case
    /** @param container
      *   container of the original and new data
      * @param func
      *   transforms original data into wrapped new data
      * @tparam F
      *   higher-kinded type for which Applicative is in scope
      * @tparam A
      *   original value type
      * @tparam B
      *   mapped value type
      * @return
      *   higher-kinded type containing new data wrapped in L
      */
    def traverse[F[_]: Applicative, A, B](container: L[A])(func: A => F[B]): F[L[B]]

    /** Unwraps the original data structure and turns it inside out
      * @param container
      *   original container
      * @tparam F
      *   higher-kinded type for which Applicative is in scope
      * @tparam A
      *   original container type
      * @return
      *   original data structure turned inside out
      */
    def sequence[F[_]: Applicative, A](container: L[F[A]]): F[L[A]] =
      traverse(container)(identity)

    // Exercise - implement this method in terms of traverse and sequence
    // hint:
    type Identity[T] = T
    // compiler can figure out Applicative[Identity] because cats already has Id type, which is the same
    // implicit Monad[Id] exists in cats, with all the methods
    def map[A, B](wa: L[A])(f: A => B): L[B] = traverse[Identity, A, B](wa)(f)
    import cats.Id
    def mapCats[A, B](wa: L[A])(f: A => B): L[B] = traverse[Id, A, B](wa)(f)

  // Traverse in cats
  import cats.Traverse
  import cats.instances.future.given // Applicative[Future]
  val allBandwidthsCats: Future[List[Int]] = Traverse[List].traverse(servers)(getBandwidth)

  // extension methods
  import cats.syntax.traverse.given // sequence, traverse methods
  val allBandwidthsCats2: Future[List[Int]] = servers.traverse(getBandwidth)

  // Summary
  // Traverse is a higher-kinded type with inside-out methods: traverse, sequence
  // Useful for
  // - turning nested data structures inside out
  // - general data combination APIs, especially with nested data structures

  def main(args: Array[String]): Unit = {
    Thread.sleep(Duration.ofMillis(1000))
    executorService.shutdown()
    println(allPairs)
    println(allTriples)
    println(allTrue)
    println(someFalse)
    println(allTrueValidated)
    println(someFalseValidated)
  }
