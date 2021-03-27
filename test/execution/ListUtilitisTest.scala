package execution

import execution.ListUtilitis._
import u03.Lists.List._
import org.junit.jupiter.api.Assertions._
import org.junit.jupiter.api.Test
import u02.Optionals.Option._


class ListUtilitisTest {


  val lst: Cons[Int] = Cons(10, Cons(20, Cons(30, Nil())))
  val lsts: Cons[String] = Cons("Uno", Cons("Due", Cons("Tre", Cons("Quattro", Nil()))))


  @Test def testDrop() {
    assertEquals(Cons(20, Cons(30, Nil())), drop(lst, 1))
    assertEquals(Cons(30, Nil()), drop(lst, 2))
    assertEquals(Nil(), drop(lst, 5))
  }


  @Test def testFlatMap() {
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), flatMapL(lst) { v =>
      Cons(v + 1, Nil())
    })
    assertEquals(Cons(11, Cons(12, Cons(21, Cons(22, Cons(31, Cons(32, Nil())))))), flatMapL(lst) { v =>
      Cons(v + 1, Cons(v + 2, Nil()))
    })
    assertEquals(Cons("uno", Cons("UNO", Cons("due", Cons("DUE", Cons("tre", Cons("TRE", Cons("quattro", Cons("QUATTRO", Nil())))))))), flatMapL(lsts) { v =>
      Cons(v.toLowerCase, Cons(v.toUpperCase, Nil()))
    })
  }

  @Test def testMap2() {
    assertEquals(Cons(12, Cons(22, Cons(32, Nil()))), map2(lst)(_ + 2))
    assertEquals(Cons(13, Cons(23, Cons(33, Nil()))), map2(lst)(_ + 3))
    assertEquals(Cons("UnoMap", Cons("DueMap", Cons("TreMap", Cons("QuattroMap", Nil())))), map2(lsts)(_.concat("Map")))

  }

  @Test def testFilter2() {
    assertEquals(Cons(20, Cons(30, Nil())), filter2[Int](lst)(_ >= 20))
    assertEquals(Cons("Uno", Cons("Due", Cons("Tre", Nil()))), filter2[String](lsts)(_.length < 4))
  }

  @Test def testMax() {
    assertEquals(Some(30), max(lst))
    assertEquals( None() , max(Nil()) )
  }


}
