package execution
import Streams.Stream
import execution.Streams._
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import u03.Lists.List._

class StreamTest {


  val s: Stream[Int] = Stream.take ( Stream.iterate (0) ( _ +1) ) (10)

  @Test def testDrop() {
    assertEquals(Cons (6 , Cons (7 , Cons (8 , Cons (9 , Nil())))), Stream.toList( Stream.drop(s)(6) ))

  }

  @Test def testConstant() {
    assertEquals(Cons ("x", Cons ("x", Cons ("x", Cons ("x", Cons ("x", Nil ()))))), Stream.toList( Stream.take ( Stream.constant("x"))(5) ))
  }

  @Test def testFib() {
    assertEquals(Cons (0 , Cons (1 , Cons (1 , Cons (2 , Cons (3 , Cons (5 , Cons (8 ,  Cons (13 , Nil ())))))))), Stream.toList( Stream.take(Stream.fibs )(8)))
  }



}
