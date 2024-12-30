package part4typeclasses

import cats.{Applicative, Monad}

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

  def main(args: Array[String]): Unit = {
    Thread.sleep(Duration.ofMillis(1000))
    executorService.shutdown()
    println(allPairs)
    println(allTriples)
  }
