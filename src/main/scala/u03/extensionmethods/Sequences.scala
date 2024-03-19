package u03.extensionmethods

object Sequences:
  
  enum Sequence[E]:
    case Cons(head: E, tail: Sequence[E])
    case Nil()

  object Sequence:

    extension (l: Sequence[Int])
      def sum: Int = l match
        case Cons(h, t) => h + t.sum
        case _          => 0

    extension [A](l: Sequence[A])

      def map[B](mapper: A => B): Sequence[B] = l match
        case Cons(h, t) => Cons(mapper(h), t.map(mapper))
        case Nil()      => Nil()

      def filter(pred: A => Boolean): Sequence[A] = l match
        case Cons(h, t) if pred(h) => Cons(h, t.filter(pred))
        case Cons(_, t)            => t.filter(pred)
        case Nil()                 => Nil()

      def concat(l2: Sequence[A]): Sequence[A] = (l, l2) match
        case (Cons(h1, t1), Cons(_, _)) => Cons(h1, t1.concat(l2))
        case (Nil(), Cons(h2, t2)) => Cons(h2, t2)
        case (Nil(), Nil()) => Nil()

      def flatMap[B](mapper: A => Sequence[B]): Sequence[B] = l match
        case Cons(h, t) if t != Nil() => mapper(h).concat(t.flatMap(mapper))
        case Cons(h, Nil()) => mapper(h)
        case Nil() => Nil()

      def foldLeft(start: A)(op: (A, A) => A): A = l match
        case Cons(h, t) if t != Nil() => t.foldLeft(op(start, h))(op)
        case Cons(h, Nil()) => op(start, h)
        case Nil() => start

      def foldRight(start: A)(op: (A, A) => A): A = l match
        case Cons(h, t) if t != Nil() => op(h, t.foldRight(start)(op))
        case Cons(h, Nil()) => op(h, start)
        case Nil() => start

    def of[A](n: Int, a: A): Sequence[A] =
      if (n == 0) then Nil[A]() else Cons(a, of(n - 1, a))

@main def trySequences() =
  import Sequences.*
  import Sequence.*
  
  val seq = Cons(10, Cons(20, Cons(30, Nil())))
  println(seq.filter(_ >= 20).map(_ + 1).sum) // 21+31 = 52
  println(sum(map(filter(seq)(_ >= 20))(_ + 1))) // equally possible
  val seq2 = of(10, -1) // Cons(-1, Cons(-1, Cons(-1, ...)))
  println(seq2.sum) // -10
  
  val lst = Cons(10, Cons(20, Cons(30, Nil())))
  println(lst.flatMap(v => Cons(v + 1, Nil()))) // Cons(11, Cons(21, Cons(31, Nil())))
  println(lst.flatMap(v => Cons(v + 1, Cons(v + 2, Nil())))) // Cons(11, Cons(12, Cons(21, Cons(22, Cons(31, Cons(32, Nil()))))))

  val lst1 = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))
  println(lst1.foldLeft(0)(_ - _)) // -16
  println(lst1.foldLeft(2)(_ + _)) // 18
  println(lst1.foldRight(0)(_ + _)) // 16
  println(lst1.foldRight(2)(_ - _)) // -6
