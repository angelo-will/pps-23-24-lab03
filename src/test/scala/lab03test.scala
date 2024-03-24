
import org.junit.*
import org.junit.Assert.*

class lab03test:
    import u03.Sequences.*
    import Sequence.*

    val l: Sequence[Int] = Cons(10, Cons(20, Cons(30, Nil())))

    @Test def testTakeWithZero() =
        assertEquals(Nil(), take(l)(0))
    
    @Test def testTakeWithMoreElements = 
        assertEquals(Cons(10,Cons(20,Nil())), take(l)(2))

    
  

