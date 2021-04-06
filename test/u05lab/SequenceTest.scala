package u05lab

import org.junit.jupiter.api.{Assertions, Test}
import u05lab.code.Lists.sequence
import u05lab.code.List

class SequenceTest {
    @Test
    def testSequence(): Unit = {
        Assertions.assertEquals(Some(List(1, 2, 3)), sequence(List[Option[Int]](Some(1), Some(2), Some(3))))
        Assertions.assertEquals(None, sequence(List[Option[Int]](Some(1), None, Some(3))))
        Assertions.assertEquals(None, sequence(List[Option[Any]](None, None, None)))
        Assertions.assertEquals(None, sequence(List[Option[Any]](None)))
        Assertions.assertEquals(Some(List(1)), sequence(List[Option[Int]](Some(1))))
        Assertions.assertEquals(None, sequence(List.nil[Option[Any]]))
    }
}
