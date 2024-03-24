package lab03

import org.junit.*
import org.junit.Assert.*

class PeopleTest:
    import u03.Sequences.*
    import Sequence.*
    import People.*
    import lab03.People.Person.*
    import u03.lab03.* 

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
