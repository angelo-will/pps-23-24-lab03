package u03
import org.junit.*
import org.junit.Assert.*

import u03.Streams.*
import Stream.*
import u03.Sequences.*
import Sequence.*


class StreamTest:

  @Test def testIterate(): Unit = 
    val str1 = Stream.iterate(0)(_ + 1) // {0,1,2,3,..}
    assertEquals(Cons(0, Cons(1, Cons(2, Cons(3, Nil())))), toList(Stream.take(str1)(4)))

  @Test def testMap(): Unit =
    val str1 = Stream.iterate(0)(_ + 1) // {0,1,2,3,..}
    val str2 = Stream.map(str1)(_ + 1) // {1,2,3,4,..}
    assertEquals(Cons(1, Cons(2, Cons(3, Cons(4, Nil())))), toList(Stream.take(str2)(4)))

  @Test def testFilter(): Unit =
    val str1 = Stream.iterate(0)(_ + 1) // {0,1,2,3,..}
    val str2 = Stream.filter(str1)(x => x % 2 == 1) // {1,3,5,7,..}
    assertEquals(Cons(1, Cons(3, Cons(5, Cons(7, Nil())))), toList(Stream.take(str2)(4)))

  @Test def takeWhile(): Unit = 
    val str1 = Stream.iterate(0)(_ + 1)
    val str2 = Stream.takeWhile(str1)(_ < 5)
    assertEquals(Cons(0, Cons(1, Cons(2, Cons(3, Cons(4, Nil()))))), Stream.toList(str2))
    
  @Test def testTakeWhileLab(): Unit = 
    val str1 = Stream.iterate(0)(_ + 1)
    val str2 = Stream.take(str1)(10)
    val strToTest = Stream.takeWhile2(str2)(4)(_ % 2 == 0)
    assertEquals(Cons(0,Cons(2,Cons(4,Cons(6,Nil())))), Stream.toList(strToTest))

  @Test def testFill(): Unit = 
    val strFill1 = Stream.fill(3)("a")
    val strFill2 = Stream.fill(2)(2)
    assertEquals(Cons("a", Cons("a", Cons("a",Nil()))), Stream.toList(strFill1))
    assertEquals(Cons(2, Cons(2,Nil())), Stream.toList(strFill2))

  @Test def testPellStream(): Unit = 
    val pell = Stream.pell()
    assertEquals(Nil(), Stream.toList(Stream.take(pell)(0)))
    assertEquals(Cons(0, Cons(1,Cons(2,Cons(5,Cons(12,Nil()))))), Stream.toList(Stream.take(pell)(5)))
