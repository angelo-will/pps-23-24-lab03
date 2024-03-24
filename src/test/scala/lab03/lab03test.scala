package lab03

import u03.Optionals.Optional.*
import org.junit.*
import org.junit.Assert.*

class SequenceTestConsegna:
  import u03.Sequences.*
  import Sequence.*

  val l: Sequence[Int] = Cons(10, Cons(20, Cons(30, Nil())))

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

  @Test def testMap2() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), map2(l)(_ + 1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), map2(l)(_ + ""))

  @Test def testFilter2() =
    assertEquals(Cons(20, Cons(30, Nil())), filter2(l)(_ >= 11))
    assertEquals(Cons(20, Cons(30, Nil())), filter2(l)(_ >= 20))
    assertEquals(Cons(10, Cons(30, Nil())), filter2(l)(_ != 20))

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

class PeopleTestConsegna:
    import u03.Sequences.*
    import Sequence.*
    import People.*
    import lab03.People.Person.*
    // import u03.lab03.*

    val teachings: Sequence[Person.Teacher] = Cons(Person.Teacher("Angelo","Matematica"), Nil())
    val getCoursesSelected: (Sequence[Person]) => Sequence[String] = 
       u03.lab03.getCoursesFlatMap
        // Person.getCourses
        // Person.getCoursesFilterWithMap

    @Test def testGetCoursesWithNoCourse() =
        val l: Sequence[Person] = 
            Cons(Person.Student("Francesco", 22),
            Cons(Person.Student("Maria", 21),
            Cons(Person.Student("Mario", 22),
            Cons(Person.Student("Angelo", 18), Nil()))))
        assertEquals(Nil(), getCoursesSelected(l))

    @Test def testGetCoursesWithOnlyCourses() =
        val l1: Sequence[Person] = 
            Cons(Person.Teacher("Francesco", "Matematica"),
            Cons(Person.Teacher("Maria", "Fisica"),
            Cons(Person.Teacher("Mario", "Algebra"),
            Cons(Person.Teacher("Angelo", "Musica"), Nil()))))
        val res1 =
            Cons("Matematica", Cons("Fisica", Cons("Algebra", Cons("Musica", Nil()))))
        assertEquals(res1, getCoursesSelected(l1))

    @Test def testGetCoursesWithMixedPeople() =
        val l1: Sequence[Person] = 
            Cons(Person.Student("Francesco", 22),
            Cons(Person.Teacher("Francesco", "Matematica"),
            Cons(Person.Student("Maria", 21),
            Cons(Person.Teacher("Maria", "Fisica"),
            Cons(Person.Teacher("Mario", "Algebra"),
            Cons(Person.Student("Mario", 22),
            Cons(Person.Teacher("Angelo", "Musica"), Nil())))))))
        val res1 =
            Cons("Matematica", Cons("Fisica", Cons("Algebra", Cons("Musica", Nil()))))
        assertEquals(res1, getCoursesSelected(l1))

class StreamTestConsegna:
  import u03.Streams.*
  import u03.Sequences.*
  import Sequence.*

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