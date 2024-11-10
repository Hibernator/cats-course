package part3datamanipulation

object Evaluation {

  /*
    Cats makes a distinction between three forms of expression evaluation
    - evaluating an expression early
    - evaluating lazily and every time it's requested
    - evaluating lazily only once and keeping the value (memoization)
   */

  import cats.Eval

  val instantEval: Eval[Int] = Eval.now {
    println("Computing now!")
    64345
  }

  val redoEval: Eval[Int] = Eval.always {
    println("Computing again!")
    4234
  }

  private val delayedEval = Eval.later {
    println("Computing later!")
    53278
  }

  // Evaluations can be composed in purely functional way via map and flatMap

  val composedEvaluation: Eval[Int] = instantEval.flatMap(value1 => delayedEval.map(value2 => value1 + value2))

  // identical to above
  val anotherComposedEvaluation: Eval[Int] = for {
    value1 <- instantEval
    value2 <- delayedEval
  } yield value1 + value2

  // Exercise 1: what gets printed to the console if this is called twice with println?
  val evalEx1 = for {
    a <- delayedEval
    b <- redoEval
    c <- instantEval
    d <- redoEval
  } yield a + b + c + d

  val dontRecompute: Eval[Int] = redoEval.memoize

  val tutorial = Eval
    .always {
      println("Step1...")
      "put the guitar on your lap"
    }
    .map { step1 =>
      println("Step2...")
      s"$step1 then put your left hand on the neck"
    }
    .memoize // stores the value up to this point, no need to recompute the whole chain anymore
    .map { steps12 => // this step will still be evaluated every time
      println("Step3, more complicated")
      s"$steps12 then with the right hand strike the strings"
    }

  // Exercise 2:
  // implement defer such that if called with Eval.now as an argument, nothing should be computed, no side effects
  def defer[T](eval: => Eval[T]): Eval[T] = Eval.later(eval.value)
  def deferDaniel[T](eval: => Eval[T]): Eval[T] = Eval.later(()).flatMap(_ => eval)

  // Exercise 3: rewrite the method with Evals
  def reverseList[T](list: List[T]): List[T] =
    if list.isEmpty then list
    else reverseList(list.tail) :+ list.head

  // solved the same way as Daniel but it is not stack-safe
  def reverseEval[T](list: List[T]): Eval[List[T]] = {
    if list.isEmpty then Eval.now(list)
    else reverseEval(list.tail).map(_ :+ list.head)
  }

  // eval chains (map, flatMap) are stack-safe (tail recursive)
  // I can use this to easily make a stack-recursive function stack-safe
  def reverseEvalStackSafe[T](list: List[T]): Eval[List[T]] = {
    if list.isEmpty then Eval.now(list)
    else Eval.defer(reverseEvalStackSafe(list.tail).map(_ :+ list.head))
  }

  def main(args: Array[String]): Unit = {
    println("Starting main")
//    println(instantEval.value)
//    println(instantEval.value)
//    println(redoEval.value)
//    println(redoEval.value)
//    println(delayedEval.value)
//    println(delayedEval.value)
//    println(composedEvaluation.value)
//    println(composedEvaluation.value)

//    println(evalEx1.value)
//    println(evalEx1.value)

//    println(dontRecompute.value)
//    println(dontRecompute.value)

//    println(tutorial.value)
//    println(tutorial.value)

//    val deferred = defer(Eval.now {
//      println("Now!")
//      42
//    })
//    println("Not evaluated yet")
//    println(deferred.value)

    val reversedListEval = reverseEval(List(1, 2, 3, 4))
    println(reversedListEval.value)
  }

}
