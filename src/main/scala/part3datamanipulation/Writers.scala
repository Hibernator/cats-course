package part3datamanipulation

import cats.Id
import cats.data.WriterT

import java.util.concurrent.Executors
import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future}

object Writers {

  // Writer is a wrapper of a value that can keep track of useful information while data/value is being manipulated

  import cats.data.Writer
  // 2 type parameters: logs, value
  // Value is manipulated and during that, I can keep track of additional information (modification sequence, logs)

  // 1 - define a writer at the start
  val aWriter: Writer[List[String], Int] = Writer(List("Started something"), 45)

  // 2 - manipulate it with pure FP
  val anIncreasedWriter = aWriter.map(_ + 1) // value increases, logs stay the same
  val aLogsWriter = aWriter.mapWritten(_ :+ "Found something interesting") // value stays the same, logs change
  val aWriterWithBothModified = aWriter.bimap(_ :+ "Found something interesting", _ + 1) // both value and logs change
  // a different way of changing both logs and value
  // slightly more useful because both logs and value can be used for each other calculation
  // in bimap they are modified completely independently of each other
  val aWriterWithBothModified2 = aWriter.mapBoth { (logs, value) => (logs :+ "Found something interesting", value + 1) }

  // Writers can be defined at the start of the app and manipulated in a purely functional way
  // At the end I can dump the logs and/or value

  // since we have map and flatMap, writers can be combined via a for-comprehension
  // logs will be combined via an implicit Semigroup, in this case vectors will be concatenated
  // Semigroup[Vector]
  import cats.instances.vector.given
  val writerA = Writer(Vector("Log A1", "Log A2"), 10)
  val writerB = Writer(Vector("Log B1"), 40)
  val compositeWriter = for {
    va <- writerA
    vb <- writerB
  } yield va + vb

  // reset the logs
  // a monoid is needed for the zero value of the logs
  import cats.instances.list.given // Monoid[List[Int]]
  val anEmptyWriter = aWriter.reset

  // 3 - dump either the value or the logs
  val desiredValue: Int = aWriter.value
  val logs: List[String] = aWriter.written
  val (l, v): (List[String], Int) = aWriter.run

  // Benefit #1: writers can deal with logs and tracking changes in a purely functional way
  // Can use them to rewrite stack-recursion into tail-recursion

  // TO DO 1: rewrite a function that "prints" things with writers
  // this a stack-recursive function
  def countAndSay(n: Int): Unit = {
    if n <= 0 then println("starting!")
    else {
      countAndSay(n - 1)
      println(n)
    }
  }

  def countAndLog(n: Int): Writer[Vector[String], Int] = {
    @tailrec
    def countAndLogWithWriter(writer: Writer[Vector[String], Int]): Writer[Vector[String], Int] = {
      if writer.value > 1 then
        countAndLogWithWriter(writer.mapBoth { (log, value) =>
          (log :+ (value - 1).toString, value - 1)
        })
      else
        writer
          .mapBoth { (log, value) =>
            (log :+ "starting!", value - 1)
          }
          .mapWritten(_.reverse)
    }
    countAndLogWithWriter(Writer(Vector[String](n.toString), n))
  }

  def countAndLogDaniel(n: Int): Writer[Vector[String], Int] = {
    if n <= 0 then Writer(Vector("starting!"), 0)
    else countAndLogDaniel(n - 1).flatMap(_ => Writer(Vector(s"$n"), n))
  }

  // TO DO 2: rewrite this method with writers
  def naiveSum(n: Int): Int = {
    if n <= 0 then 0
    else {
      println(s"Now at $n")
      val lowerSum = naiveSum(n - 1)
      println(s"Computed sum(${n - 1}) = $lowerSum")
      lowerSum + n
    }
  }

  def sumWithWriters(n: Int): Writer[Vector[String], Int] = {
    @tailrec
    def acc(
        nowWriter: Writer[Vector[String], Int],
        computedWriter: Writer[Vector[String], Int]
    ): (Writer[Vector[String], Int], Writer[Vector[String], Int]) = {
      val previousValue = nowWriter.value
      if previousValue == n then (nowWriter, computedWriter)
      else
        acc(
          nowWriter.mapBoth { (log, previousN) =>
            (log :+ s"Now at ${previousN + 1}", previousN + 1)
          },
          computedWriter.mapBoth { (log, previousSum) =>
            {
              val newSum = previousValue + 1 + previousSum
              (log :+ s"Computed sum(${previousValue + 1}) = $newSum", newSum)
            }
          }
        )
    }

    val (initialNowWriter, initialComputedWriter) =
      (Writer(Vector("Now at 0"), 0), Writer(Vector("Computed sum(0) = 0"), 0))
    val (finalNowWriter, finalComputerWriter) =
      if n <= 0 then (initialNowWriter, initialComputedWriter)
      else acc(initialNowWriter, initialComputedWriter)

    finalNowWriter.flatMap(_ => finalComputerWriter) // logs combined here
  }

  def sumWithWritersDaniel(n: Int): Writer[Vector[String], Int] = {
    if n <= 0 then Writer(Vector(), 0)
    else
      for {
        _ <- Writer(Vector(s"Now at $n"), n)
        lowerSum <- sumWithWriters(n - 1)
        _ <- Writer(Vector(s"Computed sum(${n - 1}) = $lowerSum"), n)
      } yield lowerSum + n
  }

  // Benefit #2: Writers can keep logs separate on multiple threads
  given ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

  def main(args: Array[String]): Unit = {
    println(compositeWriter.run)
    // exercise 1
    countAndSay(10)
    println(countAndLog(10).run)
    println(countAndLogDaniel(10).written.foreach(println))
    // exercise 2
    println(naiveSum(10))
    println(sumWithWriters(10).written.foreach(println))
    println(sumWithWritersDaniel(10).written.foreach(println))

    // Let's introduce concurrent calls of naiveSum and writerSum and see what happens
    // Logs of naiveSum will be interspersed
    Future(naiveSum(100)).foreach(println)
    Future(naiveSum(100)).foreach(println)

    val sumFuture1 = Future(sumWithWriters(100))
    val sumFuture2 = Future(sumWithWriters(100))
    val logs1 = sumFuture1.map(_.written)
    val logs2 = sumFuture2.map(_.written)
  }
}
