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
  val lst = (1 to 1_000_000).toList
  val vec = (1 to 1_000_000).toVector
  assert(measure("list last"){ lst.last } > measure("vector last"){ vec.last })
  assert(measure("list apply"){ lst(500_000) } > measure("vector apply"){ vec(500_000) })
  assert(measure("list append"){ lst :+ 0 } > measure("vector append"){ vec :+ 0 })
  val arr = (1 to 1_000_000).toArray
  assert(measure("list last"){ lst.last } > measure("array last"){ arr.last })
  assert(measure("list apply"){ lst(500_000) } > measure("array apply") { arr(500_000) })
  assert(measure("list prepend"){ 0 +: lst } < measure("array prepend"){ 0 +: arr })
  assert(measure("vector tail"){ vec.tail } < measure("array tail"){ arr.tail })
  assert(measure("vector prepend"){ 0 +: vec } < measure("array prepend"){ 0 +: arr })
  assert(measure("vector append"){ vec :+ 0 } < measure("array append"){ arr :+ 0 })
  println()
  import scala.collection.immutable.HashSet
  import scala.collection.immutable.TreeSet
  val hashSet = HashSet(1 to 1_000_000: _*)
  val treeSet = TreeSet(1 to 1_000_000: _*)
  assert(measure("hashSet lookup"){ hashSet.contains(1_000_000) }
         < measure("treeSet lookup"){ treeSet.contains(1_000_000) })
  assert(measure("hashSet add"){ hashSet + 0 } < measure("treeSet add"){ treeSet + 0 })
  assert(measure("hashSet min"){ hashSet.min } > measure("treeSet min"){ treeSet.min })
  import scala.collection.immutable.HashMap
  import scala.collection.immutable.TreeMap
  val hashMap = HashMap((1 to 1_000_000).zipWithIndex: _*)
  val treeMap = TreeMap((1 to 1_000_000).zipWithIndex: _*)
  assert(measure("hashMap lookup"){ hashMap.contains(1_000_000) }
         < measure("treeMap lookup"){ treeMap.contains(1_000_000) })
  assert(measure("hashMap add"){ hashMap + (0 -> 0) } < measure("treeMap add"){ treeMap + (0 -> 0) })
  assert(measure("hashMap min"){ hashMap.min } > measure("treeMap min"){ treeMap.min })
}