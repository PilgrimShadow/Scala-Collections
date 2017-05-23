package com.jgdodson.collections

import scala.collection.mutable

/**
  * A traditional multiset, which can contain numerous copies of an element
  *
  * TODO: Decide what class MultiSet should extend
  *
  * @param countMap A Map specifying the members of the MultiSet and their frequency
  * @tparam T The type of item contained in the MultiSet
  */
class MultiSet[T](val countMap: Map[T, Int]) extends Iterable[T] {

  // Check the integrity of countMap
  assert(countMap.values.forall(_ > 0), s"MultiSet initialization failed: ${countMap}")


  /**
    * The number of elements in the MultiSet
    */
  override val size: Int = countMap.values.sum


  /**
    * Construct a MultiSet from an iterable collection of items
    *
    * Auxiliary constructor
    *
    * @param items The items to include in the MultiSet
    * @return
    */
  def this(items: Iterable[T]) = this {

    val m = collection.mutable.Map[T, Int]()

    for (item <- items) {
      m(item) = m.getOrElse(item, 0) + 1
    }

    m.toMap
  }


  /**
    * Return a MultiSet with the given element added
    *
    * @param elem The element to be added
    * @return
    */
  def +(elem: T): MultiSet[T] = {
    MultiSet[T](countMap.updated(elem, countMap.getOrElse(elem, 0) + 1))
  }


  /**
    * Return a MultiSet with the given element removed
    *
    * @param elem The element to remove
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
    * @param elem The element to test for membership
    * @return
    */
  def contains(elem: T): Boolean = {
    countMap.contains(elem)
  }


  /**
    * Determine whether this MultiSet is empty
    *
    * @return
    */
  override def isEmpty: Boolean = {
    countMap.isEmpty
  }

  /**
    *
    * @return
    */
  override def nonEmpty: Boolean = {
    countMap.nonEmpty
  }

  /**
    * Return the most commonly occurring item in the MultiSet
    *
    * If there is more than one mode, only one of them will be returned.
    *
    * @return
    */
  def mode: (T, Int) = {

    countMap.maxBy(_._2)
  }

  /**
    * Return the count of the given element
    *
    * @param elem The element for which to retrieve the count
    * @return
    */
  def count(elem: T): Int = {
    countMap(elem)
  }


  /**
    * Create a set using the elements from this MultiSet
    *
    * @return
    */
  override def toSet[B >: T]: Set[B] = countMap.keysIterator.asInstanceOf[Iterator[B]].toSet


  /**
    *
    * @return
    */
  override def toString: String = s"MultiSet(${iterator.mkString(", ")})"


  /**
    * Create a new Builder for MultiSets with the same type as this
    *
    * @return
    */
  override def newBuilder: mutable.Builder[T, MultiSet[T]] = new mutables.MultiSetBuilder[T]


  /**
    * Iterate over the elements in the MultiSet
    *
    * @return
    */
  def iterator: Iterator[T] = countMap.iterator.flatMap(p => Iterator.fill(p._2)(p._1))
}

object MultiSet {

  // Convenience method
  def apply[T](): MultiSet[T] = new MultiSet[T](Map[T, Int]())

  // Convenience method
  def apply[T](countMap: Map[T, Int]): MultiSet[T] = new MultiSet[T](countMap)

  // Convenience method
  def apply[T](items: Iterable[T]): MultiSet[T] = new MultiSet[T](items)

}