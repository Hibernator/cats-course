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

      case class Frame(mapped: List[Tree[B]], todos: List[Tree[Either[A, B]]])

      // https://stackoverflow.com/questions/55042834/how-to-make-tree-mapping-tail-recursive
      def tailRec(stack: List[Frame]): Tree[B] = ???

      tailRec(List(Frame(Nil, List(f(a)))))
      stackRec(f(a))
    }
  }

  def main(args: Array[String]): Unit = {}
}
