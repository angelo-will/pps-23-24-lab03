package u03

import Optionals.Optional.*
import org.junit.*
import org.junit.Assert.*

class SequenceTest:
  import u03.Sequences.*
  import Sequence.*

  val l: Sequence[Int] = Cons(10, Cons(20, Cons(30, Nil())))

  @Test def testSum() =
    assertEquals(0, sum(Nil()))
    assertEquals(60, sum(l))

  @Test def testMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), map(l)(_ + 1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), map(l)(_ + ""))

  @Test def testFilter() =
    assertEquals(Cons(20, Cons(30, Nil())), filter(l)(_ >= 11))
    assertEquals(Cons(20, Cons(30, Nil())), filter(l)(_ >= 20))
    assertEquals(Cons(10, Cons(30, Nil())), filter(l)(_ != 20))

  @Test def testMap2() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), map2(l)(_ + 1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), map2(l)(_ + ""))

  @Test def testFilter2() =
    assertEquals(Cons(20, Cons(30, Nil())), filter2(l)(_ >= 11))
    assertEquals(Cons(20, Cons(30, Nil())), filter2(l)(_ >= 20))
    assertEquals(Cons(10, Cons(30, Nil())), filter2(l)(_ != 20))
  
  @Test def testTake() =
    assertEquals(Cons(10, Cons(20, Nil())), take(l)(2))
    assertEquals(Cons(10, Cons(20, Cons(30, Nil()))), take(l)(3))
    assertEquals(Nil(), take(l)(0))
    assertEquals(Nil(), take(Nil())(2))
  
  @Test def testZip() = 
    val l2: Sequence[String] = Cons("10", Cons("20", Cons("30", Nil())))
    assertEquals(Cons((10, "10"), Cons((20, "20"), Cons((30, "30"), Nil()))), zip(l, l2))
    assertEquals(Cons((10, "10"), Cons((20, "20"), Nil())), zip(l, take(l2)(2)))
    assertEquals(Nil(), zip(l, Nil()))
    assertEquals(Nil(), zip(Nil(), l2))
    assertEquals(Nil(), zip(Nil(), Nil()))

  @Test def testConcat() =
    val l2: Sequence[Int] = Cons(40, Cons(50, Nil()))
    assertEquals(Cons(10, Cons(20, Cons(30, Cons(40, Cons(50, Nil()))))), concat(l, l2))
    assertEquals(Cons(40, Cons(50, Cons(40, Cons(50, Nil())))), concat(l2, l2))
    assertEquals(Cons(40, Cons(50, Nil())), concat(Nil(), l2))
    assertEquals(Cons(40, Cons(50, Nil())), concat(l2, Nil()))
    assertEquals(Nil(), concat(Nil(), Nil()))

  @Test def testFlatMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), flatMap(l)(v => Cons(v + 1, Nil())))
    assertEquals(Nil(), flatMap(Nil())(v => Cons(v, Nil())))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), flatMap(l)(v => Cons("" + v, Nil())))

  @Test def testMin() =
    assertEquals(Just(10), min(l))
    assertEquals(Just(1), min(Cons(1, Nil())))
    assertEquals(Empty(), min(Nil()))

  @Test def testFoldLeftAddSubOperators() = 
    assertEquals(60, foldLeft(l)(0)(_+_))
    assertEquals(-60, foldLeft(l)(0)(_-_))
    val f: (Int,Int) => Int = _+_
    // Non riuscendo a comprendere il tipo degli elementi usando i generici,
    // ho passato la funzione dopo averne dichiarato il tipo
    assertEquals(0, foldLeft(Nil())(0)(f))

  @Test def testFoldLeftAddDivOperator() =
    val l1 = Cons(2, Cons(32, Cons(2, Nil())))
    val l2 = Cons(1, Cons(1, Nil()))
    assertEquals(1, foldLeft(l1)(128)(_/_))
    assertEquals(2, foldLeft(l2)(2)(_/_))

  @Test def FoldRightAddSubOperators() = 
    assertEquals(20, foldRight(l)(0)(_-_))

  @Test def FoldRightDivOperator()  =
    assertEquals(1, foldRight(l)(15)(_/_))