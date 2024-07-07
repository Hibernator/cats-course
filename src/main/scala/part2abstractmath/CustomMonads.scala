package part2abstractmath

import scala.annotation.tailrec
import scala.collection.mutable

object CustomMonads {

  import cats.Monad

  given optionMonad: Monad[Option] with {
    override def pure[A](x: A): Option[A] = Option(x)

    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)

    // This is needed for monad iteration/loop methods which provide iteration in a functional way
    @tailrec
    final override def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] = f(a) match {
      case None           => None
      case Some(Left(v))  => tailRecM(v)(f)
      case Some(Right(b)) => Some(b)
    }
  }

  // Exercise 1: define a monad for the identity type
  type Identity[T] = T
  val aNumber: Identity[Int] = 42

  given identityMonad: Monad[Identity] with {
    override def pure[A](x: A): Identity[A] = x

    override def flatMap[A, B](fa: Identity[A])(f: A => Identity[B]): Identity[B] = f(fa)

    @tailrec
    final override def tailRecM[A, B](a: A)(f: A => Identity[Either[A, B]]): Identity[B] = f(a) match {
      case Left(value)  => tailRecM(value)(f)
      case Right(value) => value
    }
  }

  // harder example - binary tree
  sealed trait Tree[+A]
  final case class Leaf[+A](value: A) extends Tree[A]
  final case class Branch[+A](left: Tree[A], right: Tree[A]) extends Tree[A]

  // Exercise 2: define a monad for this tree
  // tailRecM is difficult to make tail recursive

  given treeMonad: Monad[Tree] with {
    override def pure[A](x: A): Tree[A] = Leaf(x)

    override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = fa match {
      case Leaf(value)         => f(value)
      case Branch(left, right) => Branch(flatMap(left)(f), flatMap(right)(f))
    }

    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = {
      def stackRec(tree: Tree[Either[A, B]]): Tree[B] = tree match {
        case Leaf(Left(value))   => stackRec(f(value))
        case Leaf(Right(value))  => Leaf(value)
        case Branch(left, right) => Branch(stackRec(left), stackRec(right))
      }

//      case class Frame(mapped: List[Tree[B]], todos: List[Tree[Either[A, B]]])
//
//      // https://stackoverflow.com/questions/55042834/how-to-make-tree-mapping-tail-recursive
//      def tailRec(trees: List[Tree[Either[A, B]]]): Tree[B] = {
//        trees match {
//          case Leaf(Right(value)) :: tail =>
//            tail match {
//              case Nil => Leaf(value)
//              case Leaf(Right(value)) :: more =>
//              case Leaf(Left(value)) :: more =>
//              case Branch(left, right) :: more =>
//            }
//          case Leaf(Left(value)) :: tail  =>
//          case Branch(left, right) :: tail =>
//        }
//      }

//      tailRec(List(f(a)))

      /*
        assume that f(a) returns the following tree

                _____1_____
             __2__       __3__
            /     \     /     \
           L1     R2   R3     R4

          B4 - value obtained from R4

        tr([1], [], []) =
        tr([3, 2, 1], [1], []) =
        tr([R4, R3, 3, 2, 1], [1, 3], []) =
        tr([R3, 3, 2, 1], [1, 3], [B4]) =
        tr([3, 2, 1], [1, 3], [B3, B4]) =
        tr([2, 1], [1, 3], [B34]) =
        tr([R2, L1, 2, 1], [1, 2, 3], [B34]) =
        tr([L1, 2, 1], [1, 2, 3], [B2, B34]) =
        tr([R1, 2, 1], [1, 2, 3], [B2, B#$]) =
        tr([2, 1], [1, 2, 3], [B1, B2, B34]) =
        tr([1], [1, 2, 3], [B12, B34]) =
        tr([], [1, 2, 3], [B1234]) =
        B1234
       */
      def tailRec(todo: List[Tree[Either[A, B]]], expanded: Set[Tree[Either[A, B]]], done: List[Tree[B]]): Tree[B] =
        if (todo.isEmpty) done.head
        else
          todo.head match {
            case Leaf(Left(v))  => tailRec(f(v) :: todo.tail, expanded, done)
            case Leaf(Right(b)) => tailRec(todo.tail, expanded, Leaf(b) :: done)
            case node @ Branch(left, right) =>
              if (!expanded.contains(node)) {
                tailRec(right :: left :: todo, expanded + node, done)
              } else {
                val newLeft = done.head
                val newRight = done.tail.head
                val newBranch = Branch(newLeft, newRight)
                tailRec(todo.tail, expanded, newBranch :: done.drop(2))
              }
          }

      tailRec(List(f(a)), Set(), List())
//      stackRec(f(a))
    }
  }

  def main(args: Array[String]): Unit = {
    val tree: Tree[Int] = Branch(Leaf(10), Leaf(20))
    val changedTree = treeMonad.flatMap(tree)(v => Branch(Leaf(v + 1), Leaf(v + 2)))
    println(changedTree)
  }
}
