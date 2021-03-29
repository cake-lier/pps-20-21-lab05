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
}
