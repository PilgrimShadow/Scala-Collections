package com.jgdodson.collections

class MultiSet[T](val countMap: Map[T, Int]) extends Set[T] {

  // Auxiliary constructor
  def this(items: Iterable[T]) = this {

    val m = collection.mutable.Map[T, Int]()

    for (item <- items) {
      m(item) = m.getOrElse(item, 0) + 1
    }

    m.toMap
  }


  /**
    * Add an element to the MultiSet
    *
    * @param elem
    * @return
    */
  def +(elem: T): MultiSet[T] = {
    MultiSet[T](countMap.updated(elem, countMap.getOrElse(elem, 0) + 1))
  }


  /**
    * Remove an element from the MultiSet
    *
    * @param elem
    * @return
    */
  def -(elem: T): MultiSet[T] = {
    if (countMap.contains(elem)) {
      if (countMap(elem) > 1) {
        MultiSet(countMap.updated(elem, countMap(elem) - 1))
      } else {
        MultiSet(countMap - elem)
      }
    } else {
      this
    }
  }


  /**
    * Determine whether the MultiSet contains an item
    *
    * @param elem
    * @return
    */
  def contains(elem: T): Boolean = {
    countMap.contains(elem)
  }


  /**
    * Iterate over the elements in the MultiSet
    *
    * @return
    */
  def iterator: Iterator[T] = countMap.iterator.flatMap(p => Iterator.fill(p._2)(p._1))
}

object MultiSet {

  def apply[T](countMap: Map[T, Int]): MultiSet[T] = new MultiSet[T](countMap)

  def apply[T](items: Iterable[T]): MultiSet[T] = new MultiSet[T](items)

}