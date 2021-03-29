package u05lab

import org.junit.jupiter.api.{Assertions, Test}
import u05lab.code.List

class ListsTest {
    private val l = List("a", "b", "c", "a")
    private val pred = "a" == _
    private val alwaysFalsePred = "z" == _

    @Test
    def testListRight(): Unit = {
        Assertions.assertEquals(List.nil, List.nil.zipRight)
        Assertions.assertEquals(List(("a", 0), ("b", 1), ("c", 2), ("a", 3)), this.l.zipRight)
    }

    @Test
    def testPartition(): Unit = {
        Assertions.assertEquals((List.nil, List.nil), List.nil.partition(this.pred))
        Assertions.assertEquals((List("a", "a"), List("b", "c")), this.l.partition(this.pred))
        Assertions.assertEquals((List.nil, this.l), this.l.partition(this.alwaysFalsePred))
        Assertions.assertEquals((this.l, List.nil), this.l.partition(!this.alwaysFalsePred(_)))
    }

    @Test
    def testSpan(): Unit = {
        Assertions.assertEquals((List.nil, List.nil), List.nil.span(this.pred))
        Assertions.assertEquals((List("a"), List("b", "c", "a")), this.l.span(this.pred))
        Assertions.assertEquals((List.nil, this.l), this.l.span(this.alwaysFalsePred))
        Assertions.assertEquals((this.l, List.nil), this.l.span(!this.alwaysFalsePred(_)))
    }

    @Test
    def testReduce(): Unit = {
        val s = List("a")
        val fun: (String, String) => String = _ + _
        Assertions.assertThrows(classOf[UnsupportedOperationException], () => List.nil.reduce(fun))
        Assertions.assertEquals("a", s.reduce(fun))
        Assertions.assertEquals("abca", this.l.reduce(fun))
    }

    @Test
    def testTakeRight(): Unit = {
        val biggerThanSize = 5
        val n = 2
        Assertions.assertEquals(this.l, this.l.takeRight(biggerThanSize))
        Assertions.assertEquals(List.nil, this.l.takeRight(0))
        Assertions.assertEquals(List("c", "a"), this.l.takeRight(n))
        Assertions.assertEquals(List.nil, List.nil.takeRight(0))
        Assertions.assertEquals(List.nil, List.nil.takeRight(n))
    }

    @Test
    def testCollect(): Unit = {
        val fun: PartialFunction[String, Int] = {
            case x if this.pred(x) => 0
        }
        val alwaysFalseFun: PartialFunction[String, Int] = {
            case x if this.alwaysFalsePred(x) => 0
        }
        val alwaysTrueFun: PartialFunction[String, Int] = {
            case x if !this.alwaysFalsePred(x) => 0
        }
        Assertions.assertEquals(List.nil, List.nil.collect(fun))
        Assertions.assertEquals(List(0, 0), this.l.collect(fun))
        Assertions.assertEquals(List.nil, this.l.collect(alwaysFalseFun))
        Assertions.assertEquals(this.l.map(_ => 0), this.l.collect(alwaysTrueFun))
    }
}
