package u03

import u02.AnonymousFunctions.l
import u03.Optionals.Optional
import u03.Persons.*
import u03.Persons.Person.isStudent
import u03.Persons.Person.getCourseName
import u03.Optionals.Optional.isEmpty
import u03.Optionals.Optional.orElse

object Sequences: // Essentially, generic linkedlists
  
  enum Sequence[E]:
    case Cons(head: E, tail: Sequence[E])
    case Nil()

  object Sequence:

    def sum(l: Sequence[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _          => 0

    def map[A, B](l: Sequence[A])(mapper: A => B): Sequence[B] = l match
      case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
      case Nil()      => Nil()

    def map2[A, B](l: Sequence[A])(mapper: A => B): Sequence[B] = flatMap(l)(a => Cons(mapper(a), Nil()))

    def filter[A](l1: Sequence[A])(pred: A => Boolean): Sequence[A] = l1 match
      case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
      case Cons(_, t)            => filter(t)(pred)
      case Nil()                 => Nil()

    //def filter2[A](l1: Sequence[A])(pred: A => Boolean): Sequence[A] = filter(flatMap(l1)(a => l1))(pred)

    // Lab 03
    def zip[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] = (first, second) match
      case (Cons(h1, t1), Cons(h2, t2)) => Cons((h1, h2), zip(t1, t2))
      case _ => Nil()

    def take[A](l: Sequence[A])(n: Int): Sequence[A] = l match
      case Cons(h, t) if n > 0 => Cons(h, take(t)(n-1))
      case _ => Nil()
    
    def concat[A](l1: Sequence[A], l2: Sequence[A]): Sequence[A] = (l1, l2) match
      case (Cons(h1, t1), Cons(_, _)) => Cons(h1, concat(t1, l2))
      case (Nil(), Cons(h2, t2)) => Cons(h2, t2)
      case (Nil(), Nil()) => Nil()

    def flatMap[A, B](l: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] = l match
      case Cons(h, t) if t != Nil() => concat(mapper(h), flatMap(t)(mapper))
      case Cons(h, Nil()) => mapper(h)
      case Nil() => Nil()

    def min(l: Sequence[Int]): Optional[Int] = 
      def _min(l: Sequence[Int], min: Optional[Int]): Optional[Int] = l match
        case Cons(h, t) if isEmpty(min) | h < orElse(min, 100) => _min(t, Optional.Just(h))
        case Cons(_, t) => _min(t, min)
        case _ => min
      _min(l, Optional.Empty())
    
    def getCourses(l: Sequence[Person]): Sequence[String] = map(filter(l)(!isStudent(_)))(p => getCourseName(p))

    def getCourses2(l: Sequence[Person]): Sequence[String] = flatMap(filter(l)(!isStudent(_)))(p => Cons(getCourseName(p), Nil()))
    
@main def trySequences =
  import Sequences.* 
  import Persons.* 
  val l = Sequence.Cons(10, Sequence.Cons(20, Sequence.Cons(30, Sequence.Nil())))
  println(Sequence.sum(l)) // 60

  import Sequence.*

  println(sum(map(filter(l)(_ >= 20))(_ + 1))) // 21+31 = 52
  println(sum(map2(filter(l)(_ >= 20))(_ + 5))) // 25+35 = 60

  val lst = Cons(10, Cons(20, Cons(30, Nil())))
  println(take(lst)(2)) // Cons(10, Cons (20, Nil()))
  println(take(lst)(0)) // Nil()
  println(take(lst)(5)) // Cons(10, Cons(20, Cons(30, Nil())))

  val lst1 = Cons(10, Cons(20, Cons(30, Nil())))
  val lst2 = Cons("a", Cons("b", Cons("c", Nil())))
  println(zip(lst1, lst2)) // Cons((10, a), Cons((20, b), Cons((30, c), Nil())))

  val lst3 = Cons(10, Cons(20, Nil()))
  val lst4 = Cons(30, Cons(40, Nil()))
  println(concat(lst3, lst4)) // Cons(10, Cons(20, Cons(30, Cons(40, Nil()))))
  
  println(flatMap(lst)(v => Cons(v + 1, Nil()))) // Cons(11, Cons(21, Cons(31, Nil())))
  println(flatMap(lst)(v => Cons(v + 1, Cons(v + 2, Nil())))) // Cons(11, Cons(12, Cons(21, Cons(22, Cons(31, Cons(32, Nil()))))))

  println(min(Cons(10, Cons(25, Cons(20, Nil()))))) // Maybe(10)

  val p = Sequence.Cons(Person.Teacher("name", "Maths"), Cons(Person.Teacher("name", "IT"),
   Cons(Person.Teacher("name", "Italian"), Cons(Person.Student("name", 2001), Cons(Person.Teacher("name", "Science"), Nil())))))
  println(getCourses(p))
  println(getCourses2(p))