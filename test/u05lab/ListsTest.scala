package u05lab

import org.junit.jupiter.api.{Assertions, Test}
import u05lab.code.List

class ListsTest {
    @Test
    def testListRight(): Unit = {
        val l = List("a", "b", "c")
        Assertions.assertEquals(List.nil, List.nil.zipRight)
        Assertions.assertEquals(List(("a", 0), ("b", 1), ("c", 2)), l.zipRight)
    }

    @Test
    def testPartition(): Unit = {
        val l = List("a", "b", "c")
        val pred = "a" == _
        val alwaysPred = "z" == _
        Assertions.assertEquals((List.nil, List.nil), List.nil.partition(pred))
        Assertions.assertEquals((List("a"), List("b", "c")), l.partition(pred))
        Assertions.assertEquals((List.nil, l), l.partition(alwaysPred))
        Assertions.assertEquals((l, List.nil), l.partition(!alwaysPred(_)))
    }

    @Test
    def testSpan(): Unit = {
        val l = List("a", "b", "c", "a")
        val pred = "a" == _
        val alwaysPred = "z" == _
        Assertions.assertEquals((List.nil, List.nil), List.nil.span(pred))
        Assertions.assertEquals((List("a"), List("b", "c", "a")), l.span(pred))
        Assertions.assertEquals((List.nil, l), l.span(alwaysPred))
        Assertions.assertEquals((l, List.nil), l.span(!alwaysPred(_)))
    }

    @Test
    def testReduce(): Unit = {
        val s = List("a")
        val l = List("a", "b", "c", "a")
        val fun: (String, String) => String = _ + _
        Assertions.assertThrows(classOf[UnsupportedOperationException], () => List.nil.reduce(fun))
        Assertions.assertEquals("a", s.reduce(fun))
        Assertions.assertEquals("abca", l.reduce(fun))
    }
}
