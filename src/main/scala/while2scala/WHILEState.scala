package while2scala

import scala.collection.mutable.HashMap
import scala.collection.mutable.ListMap

class WHILEState(values: Seq[Long]) {

  val map = {
    val map = new HashMap[Int, Long]
    var key: Int = 1
    for (value <- values) {
      map.put(key, value)
      key += 1
    }
    map
  }

  def apply(key: Int): Long = map.getOrElse(key, 0)

  def +(key: Int, value: Long): WHILEState = {
    map.put(key, value)
    this
  }

  override def toString: String = {
    println(List[Int](3, 5, 4, 2, 6, 8, 2, 6).sortBy(i => i))
    val x = ListMap[Int, Long](map.toList.sortBy(kv => kv._1.toInt): _*)
    println(x)
    x.foldLeft("[")((s: String, kv: (Int, Long)) => s + ", x" + kv._1 + "=" + kv._2) + "]"
  }

}