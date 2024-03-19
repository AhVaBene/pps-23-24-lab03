package u03

import scala.math._
import u03.Streams.Stream.pellIterate

object Streams extends App :

  import Sequences.*

  enum Stream[A]:
    private case Empty()
    private case Cons(head: () => A, tail: () => Stream[A])

  object Stream:

    def empty[A](): Stream[A] = Empty()

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)

    def toList[A](stream: Stream[A]): Sequence[A] = stream match
      case Cons(h, t) => Sequence.Cons(h(), toList(t()))
      case _ => Sequence.Nil()

    def map[A, B](stream: Stream[A])(f: A => B): Stream[B] = stream match
      case Cons(head, tail) => cons(f(head()), map(tail())(f))
      case _ => Empty()

    def filter[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match
      case Cons(head, tail) if (pred(head())) => cons(head(), filter(tail())(pred))
      case Cons(head, tail) => filter(tail())(pred)
      case _ => Empty()

    def take[A](stream: Stream[A])(n: Int): Stream[A] = (stream, n) match
      case (Cons(head, tail), n) if n > 0 => cons(head(), take(tail())(n - 1))
      case _ => Empty()

    def iterate[A](init: => A)(next: A => A): Stream[A] =
      cons(init, iterate(next(init))(next))

    def pellIterate(n1: => Int)(n2: => Int): Stream[Int] = n2 match
      case -1 => cons(0, pellIterate(0)(0))
      case 0 => cons(1, pellIterate(0)(1))
      case _ =>
        val n = n2 * 2 + n1
        cons(n, pellIterate(n2)(n))

    // Task 3

    def takeWhile[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match
      case Cons(head, tail) if pred(head()) => cons(head(), takeWhile(tail())(pred))
      case _ => Empty()

    def fill[A](n: Int)(k: A): Stream[A] =
      def _fill(s: Stream[A])(n: Int)(k: A): Stream[A] = n match
        case n if n > 0 => _fill(cons(k, s))(n - 1)(k)
        case _ => s
      _fill(empty())(n)(k)

  end Stream

@main def tryStreams =
  import Streams.* 

  val str1 = Stream.iterate(0)(_ + 1) // {0,1,2,3,..}
  val str2 = Stream.map(str1)(_ + 1) // {1,2,3,4,..}
  val str3 = Stream.filter(str2)(x => (x < 3 || x > 20)) // {1,2,21,22,..}
  val str4 = Stream.take(str3)(10) // {1,2,21,22,..,28}
  println(Stream.toList(str4)) // [1,2,21,22,..,28]

  lazy val corec: Stream[Int] = Stream.cons(1, corec) // {1,1,1,..}
  println(Stream.toList(Stream.take(corec)(10))) // [1,1,..,1]

  val s = Stream.iterate(0)(_ + 1)
  println(Stream.toList(Stream.takeWhile(s)(_ < 5))) // Cons(0, Cons(1, Cons(2, Cons(3, Cons(4, Nil())))))

  println(Stream.toList(Stream.fill(3)("a"))) // Cons(a, Cons(a, Cons(a, Nil())))

  val pell: Stream[Int] = pellIterate(0)(-1) // 0, 1, 2, 5, 12, 29, 70, 169, 408, 985, 2378, 5741, 13860...
  println(Stream.toList(Stream.take(pell)(10))) // Cons(0, Cons(1, Cons(2, Cons(5, Cons(12, Nil()))))