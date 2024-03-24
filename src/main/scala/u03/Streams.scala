// Task 3

package u03

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
      
    def takeWhile[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match
      case Cons(head, tail) if pred(head()) => cons(head(), takeWhile(tail())(pred))
      case _ => Empty()

    def takeWhile2[A](s: Stream[A])(n: Int)(pred: A => Boolean): Stream[A] = 
      take(filter(s)(pred))(n)

    def fill[A](n: Int)(v: A): Stream[A] = n match
      case _ if n > 0 => cons(v, fill(n-1)(v))
      case _ => Empty()
      
    def pell(): Stream[Int] = 
      def _pell(n1: Int, n2: Int): Stream[Int] = 
        val x = 2*n1 + n2  
        cons(x,_pell(x, n1))
      cons(0, cons(1,  _pell(1,0)))
      
  end Stream

@main def tryStreams =
  import Streams.* 

  val str1 = Stream.iterate(0)(_ + 1) // {0,1,2,3,..}
  val str2 = Stream.map(str1)(_ + 1) // {1,2,3,4,..}
  val str3 = Stream.filter(str2)(x => (x < 3 || x > 20)) // {1,2,21,22,..}
  val str4 = Stream.take(str3)(10) // {1,2,21,22,..,28}
  // val pell = Stream.pell()
  println(Stream.toList(str4)) // [1,2,21,22,..,28]
  // println(Stream.toList(Stream.takeWhile2(str2)(10)(_%2 == 0)))
  // println(Stream.toList(Stream.take(pell)(0)))
  // println(Stream.toList(Stream.take(pell)(0)))
  // println(Stream.toList(Stream.take(pell)(3)))
  // println(Stream.toList(Stream.take(pell)(3)))
  // println(Stream.toList(Stream.take(pell)(5)))
  // println(Stream.toList(Stream.take(pell)(5)))

  lazy val corec: Stream[Int] = Stream.cons(1, corec) // {1,1,1,..}
  println(Stream.toList(Stream.take(corec)(10))) // [1,1,..,1]