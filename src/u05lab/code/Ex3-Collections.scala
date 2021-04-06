package u05lab.code

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration

object PerformanceUtils {
  case class MeasurementResults[T](result: T, duration: FiniteDuration) extends Ordered[MeasurementResults[_]] {
    override def compare(that: MeasurementResults[_]): Int = duration.toNanos.compareTo(that.duration.toNanos)
  }

  def measure[T](msg: String)(expr: => T): MeasurementResults[T] = {
    val startTime = System.nanoTime()
    val res = expr
    val duration = FiniteDuration(System.nanoTime() - startTime, TimeUnit.NANOSECONDS)
    if (msg.nonEmpty) println(msg + " -- " + duration.toNanos + " nanos; " + duration.toMillis + "ms")
    MeasurementResults(res, duration)
  }

  def measure[T](expr: => T): MeasurementResults[T] = measure("")(expr)

  def testMeasurements[T](value1: T, name1: String)
                         (value2: T, name2: String)
                         (checkOp: (MeasurementResults[_], MeasurementResults[_]) => Boolean)
                         (testMeth: T => _, nameOp: String): Unit
    = assert(checkOp(measure(name1 + " " + nameOp)(testMeth(value1)), measure(name2 + " " + nameOp)(testMeth(value2))))
}

object CollectionsTest extends App {
  /* Linear sequences: List, ListBuffer */
  import scala.collection.immutable.List
  //Create
  var immList: List[Int] = List(10, 20, 30)
  println(immList)
  //Read
  println(immList(1))
  //Update
  immList = immList.updated(1, 15)
  println(immList)
  //Delete
  println(immList.diff(List(15)))

  import scala.collection.mutable.ListBuffer
  //Create
  var mutList: ListBuffer[Int] = ListBuffer(10, 20, 30)
  println(mutList)
  //Read
  println(mutList(1))
  //Update
  mutList(1) = 15
  println(mutList)
  //Delete
  mutList -= 15
  println(mutList)

  /* Indexed sequences: Vector, Array, ArrayBuffer */
  import scala.collection.immutable.ArraySeq
  //Create
  var immArr: ArraySeq[Int] = ArraySeq(10, 20, 30)
  println(immArr)
  //Read
  println(immArr(1))
  //Update
  immArr = immArr.updated(1, 15)
  println(immArr)
  //Delete
  println(immArr.diff(List(15)))

  import scala.collection.mutable.ArrayBuffer
  //Create
  var mutArr: ArrayBuffer[Int] = ArrayBuffer(10, 20, 30)
  println(mutArr)
  //Read
  println(mutArr(1))
  //Update
  mutArr(1) = 15
  println(mutArr)
  //Delete
  println(mutArr.diff(List(15)))

  /* Sets */
  import scala.collection.immutable.{HashSet => ImmutableHashSet}
  //Create
  var immSet: ImmutableHashSet[Int] = ImmutableHashSet(10, 20, 30)
  println(immSet)
  //Read
  println(immSet(1))
  //Update
  immSet += 15
  println(immSet)
  //Delete
  immSet -= 15
  println(immSet)

  import scala.collection.mutable.{HashSet => MutableHashSet}
  //Create
  var mutSet: MutableHashSet[Int] = MutableHashSet(10, 20, 30)
  println(mutSet)
  //Read
  println(mutSet(1))
  //Update
  mutSet = mutSet.union(MutableHashSet(15))
  println(mutSet)
  //Delete
  mutSet = mutSet.diff(MutableHashSet(15))
  println(mutSet)

  /* Maps */
  import scala.collection.immutable.{HashMap => ImmutableHashMap}
  //Create
  var immMap: ImmutableHashMap[Int, String] = ImmutableHashMap(10 -> "a", 20 -> "b", 30 -> "c")
  println(immMap)
  //Read
  println(immMap(10))
  //Update
  immMap += (15 -> "d")
  println(immMap)
  //Delete
  immMap -= 15
  println(immMap)

  import scala.collection.mutable.{HashMap => MutableHashMap}
  //Create
  var mutMap: MutableHashMap[Int, String] = MutableHashMap(10 -> "a", 20 -> "b", 30 -> "c")
  println(mutMap)
  //Read
  println(mutMap(10))
  //Update
  mutMap += (15 -> "d")
  println(mutMap)
  //Delete
  mutMap -= 15
  println(mutMap)
  println()

  /* Comparison */
  import PerformanceUtils._
  //Sequences
  val lst = (1 to 1_000_000).toList
  val vec = (1 to 1_000_000).toVector
  val testListSlowerThanVector: (Seq[Int] => _, String) => Unit
    = testMeasurements[Seq[Int]](lst, "list")(vec, "vector")(_ > _)
  testListSlowerThanVector(_.last, "last")
  testListSlowerThanVector(_.apply(500_000), "apply")
  testListSlowerThanVector(_.:+(0), "append")
  val arr = (1 to 1_000_000).toArray
  val testListAndArray: ((MeasurementResults[_], MeasurementResults[_]) => Boolean) => (Seq[Int] => _, String) => Unit
      = testMeasurements[Seq[Int]](lst, "list")(arr, "array")
  testListAndArray(_ > _)(_.last, "last")
  testListAndArray(_ > _)(_.apply(500_000), "apply")
  testListAndArray(_ < _)(_.+:(0), "prepend")
  val testArraySlowerThanVector: (Seq[Int] => _, String) => Unit
    = testMeasurements[Seq[Int]](arr, "array")(vec, "vector")(_ > _)
  testArraySlowerThanVector(_.tail, "tail")
  testArraySlowerThanVector(_.+:(0), "prepend")
  testArraySlowerThanVector(_.:+(0), "append")
  //Sets
  import scala.collection.immutable.{HashSet, TreeSet}
  val hashSet = HashSet(1 to 1_000_000: _*)
  val treeSet = TreeSet(1 to 1_000_000: _*)
  val testHashSetAndTreeSet: ((MeasurementResults[_], MeasurementResults[_]) => Boolean) => (Set[Int] => _, String) => Unit
    = testMeasurements[Set[Int]](hashSet, "hashSet")(treeSet, "treeSet")
  testHashSetAndTreeSet(_ < _)(_.contains(1_000_000), "lookup")
  testHashSetAndTreeSet(_ < _)(_.+(0), "add")
  testHashSetAndTreeSet(_ > _)(_.min, "min")
  // Maps
  import scala.collection.immutable.{HashMap, TreeMap}
  val hashMap = HashMap((1 to 1_000_000).zipWithIndex: _*)
  val treeMap = TreeMap((1 to 1_000_000).zipWithIndex: _*)
  val testHashMapAndTreeMap: ((MeasurementResults[_], MeasurementResults[_]) => Boolean) => (Map[Int, Int] => _, String) => Unit
    = testMeasurements[Map[Int, Int]](hashMap, "hashMap")(treeMap, "treeMap")
  testHashMapAndTreeMap(_ < _)(_.contains(1_000_000), "lookup")
  testHashMapAndTreeMap(_ < _)(_.+(0 -> 0), "add")
  testHashMapAndTreeMap(_ > _)(_.min, "min")
}