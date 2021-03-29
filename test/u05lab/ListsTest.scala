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
}
