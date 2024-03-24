package u03

import u03.Sequences.Sequence
import u03.Sequences.Sequence.*
import u03.Optionals.Optional
import u03.Streams.Stream
import u03.Streams.Stream.*
import _root_.lab03.People.Person
import _root_.lab03.People.Person.*

object lab03:
    
    // TASK 1.A
    def take[A](l: Sequence[A])(n: Int): Sequence[A] = l match
      case Cons(h, t) if n > 0 => Cons(h, take(t)(n-1))
      case _                   => Nil()
    
    // TASK 1.B
    def zip[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] = (first,second) match
      case (Cons(h1, t1), Cons(h2, t2)) => Cons((h1, h2), zip(t1, t2))
      case _                            => Nil()

    // TASK1.C
    def concat[A](l1: Sequence[A], l2: Sequence[A]): Sequence[A] = (l1, l2) match 
      case (Cons(h, t), _)  => Cons(h, concat(t, l2))
      case (_, Cons(h, t))  => Cons(h, t)
      case _                => Nil()

    // TASK 1.D
    def flatMap[A, B](l: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] = 
      @annotation.tailrec
      def _flatMap[A, B](l: Sequence[A])(mapper: A => Sequence[B])(acc: Sequence[B]): Sequence[B] = l match
        case Cons(h, t) => _flatMap(t)(mapper)(concat(acc, mapper(h)))
        case _          => acc
      _flatMap(l)(mapper)(Nil())

    // TASK 1.E
    def map2[A, B](l: Sequence[A])(mapper: A => B): Sequence[B] = flatMap(l)(v => Cons(mapper(v), Nil()))

    def filter2[A](l1: Sequence[A])(pred: A => Boolean): Sequence[A] = flatMap(l1)(v => v match
      case _ if pred(v) => Cons(v, Nil())
      case _            => Nil()
    )

    // TASK 2
    def min(l: Sequence[Int]): Optional[Int] = l match
      case Cons(h1, Cons(h2, t)) if h1 <= h2  => min(Cons(h1, t))
      case Cons(_, Cons(h2, t))               => min(Cons(h2, t))
      case Cons(h, _)                         => Optional.Just(h)
      case _                                  => Optional.Empty()

    // TASK 3
    def getCoursesFlatMap(p: Sequence[Person]): Sequence[String] = 
            flatMap(p)(v => v match
                case Person.Teacher(n, c)   => Cons(c, Nil())
                case _                      => Nil()
            )

    def getCourses(p: Sequence[Person]): Sequence[String] = p match
        case Cons(Teacher(name, course), t) => Cons(course, getCourses(t))
        case Cons(Student(_, _), t)         => getCourses(t)
        case _ => Nil()
        
    def getCoursesFilterWithMap(p: Sequence[Person]): Sequence[String] =
        val prof = Sequence.filter(p)(_ match
            case Person.Teacher(_,_)    => true
            case _                      => false
        )
        Sequence.map(prof)(v => v match 
            case Person.Teacher(n,c) => c
        )

    // TASK 4
    @annotation.tailrec
    def foldLeft[A, B](l: Sequence[A])(acc: B)(f: (B, A) => B): B = l match
      case Cons(h, t)   => foldLeft(t)(f(acc, h))(f)
      case _            => acc

    def foldRight[A, B](l: Sequence[A])(acc: B)(f: (A, B) => B): B = l match
      case Cons(h, t)   => f(h, foldRight(t)(acc)(f))
      case _            => acc

    // TASK 5
    // NON PRESENTE
    
    // TASK 6
    // def takeWhile[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match
    //   case Cons(head, tail) if pred(head()) => cons(head(), takeWhile(tail())(pred))
    //   case _ => empty()

    def takeWhile2[A](s: Stream[A])(n: Int)(pred: A => Boolean): Stream[A] = 
      Streams.Stream.take(Streams.Stream.filter(s)(pred))(n)

    // // TASK 7
    def fill[A](n: Int)(v: A): Stream[A] = n match
      case _ if n > 0   => cons(v, fill(n-1)(v))
      case _            => empty()
      
    // TASK 8
    def pell(): Stream[Int] = 
      def _pell(n1: Int, n2: Int): Stream[Int] = 
        val x = 2*n1 + n2  
        cons(x,_pell(x, n1))
      cons(0, cons(1,  _pell(1,0)))
