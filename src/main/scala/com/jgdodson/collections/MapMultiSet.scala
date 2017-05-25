package com.jgdodson.collections

import scala.collection.mutable

/**
  * A MultiSet backed by a Map
  *
  * @param counts A Map specifying the members of the MultiSet and their frequency
  * @tparam T The type of item contained in the MultiSet
  */
class MapMultiSet[T](counts: Map[T, Int]) extends MultiSet[T] {

  // Check the integrity of countMap
  assert(counts.values.forall(_ > 0), s"MultiSet initialization failed: ${counts}")


  /**
    * The number of elements in the MultiSet
    */
  override val size: Int = counts.values.sum


  /**
    * Return a Map containing the counts of all elements
    *
    * @return
    */
  def countMap: Map[T, Int] = counts


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
  def +(elem: T): MapMultiSet[T] = {
    MapMultiSet[T](counts.updated(elem, counts.getOrElse(elem, 0) + 1))
  }


  /**
    * Return a MultiSet with the given element removed
    *
    * @param elem The element to remove
    * @return
    */
  def -(elem: T): MapMultiSet[T] = {
    if (counts.contains(elem)) {
      if (counts(elem) > 1) {
        MapMultiSet(counts.updated(elem, counts(elem) - 1))
      } else {
        MapMultiSet(counts - elem)
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
    counts.contains(elem)
  }


  /**
    * Determine whether this MultiSet is empty
    *
    * @return
    */
  override def isEmpty: Boolean = {
    counts.isEmpty
  }

  /**
    *
    * @return
    */
  override def nonEmpty: Boolean = {
    counts.nonEmpty
  }


  /**
    * Sum the elements of this MultiSet, assuming they are Numeric
    *
    * @param num
    * @tparam B The type of the resultant sum
    * @return
    */
  override def sum[B >: T](implicit num: Numeric[B]): B = counts.foldLeft(num.zero)((acc, next) => num.plus(acc, num.times(next._1, num.fromInt(next._2))))


  /**
    * Return the maximum element in the MultiSet
    *
    * @param cmp
    * @tparam B The type of the resultant max
    * @return
    */
  override def max[B >: T](implicit cmp: Ordering[B]): T = counts.keysIterator.max[B]


  /**
    * Return the maximum element, after applying the given transformation
    *
    * @param f A function to apply to elements before comparing them
    * @param cmp
    * @tparam B The type of the resultant max
    * @return
    */
  override def maxBy[B](f: (T) => B)(implicit cmp: Ordering[B]): T = counts.keysIterator.maxBy[B](f)


  /**
    * Return the minimum element in the MultiSet
    *
    * @param cmp
    * @tparam B The type of the resultant min
    * @return
    */
  override def min[B >: T](implicit cmp: Ordering[B]): T = counts.keysIterator.min[B]


  /**
    * Return the minimum element, after applying the given transformation
    *
    * @param f A function to apply to elements before comparing them
    * @param cmp
    * @tparam B The type of the resultant min
    * @return
    */
  override def minBy[B](f: (T) => B)(implicit cmp: Ordering[B]): T = counts.keysIterator.minBy[B](f)


  /**
    * Return the most commonly occurring item in the MultiSet
    *
    * If there is more than one mode, only one of them will be returned.
    *
    * @return
    */
  def mode: (T, Int) = {

    counts.maxBy(_._2)
  }


  /**
    * Return the count of the given element
    *
    * @param elem The element for which to retrieve the count
    * @return
    */
  def count(elem: T): Int = {
    counts(elem)
  }


  /**
    * Create a set using the elements from this MultiSet
    *
    * @return
    */
  override def toSet[B >: T]: Set[B] = counts.keysIterator.asInstanceOf[Iterator[B]].toSet


  /**
    * Return the String representation of this MultiSet
    *
    * @return
    */
  override def toString: String = s"MultiSet(${iterator.mkString(", ")})"


  /**
    * Create a new Builder for MultiSets with the same type as this
    *
    * @return
    */
  override def newBuilder: mutable.Builder[T, MapMultiSet[T]] = new mutables.MultiSetBuilder[T]


  /**
    * Iterate over the elements in the MultiSet
    *
    * @return
    */
  def iterator: Iterator[T] = counts.iterator.flatMap(p => Iterator.fill(p._2)(p._1))
}

object MapMultiSet {

  // Convenience method
  def apply[T](countMap: Map[T, Int]): MapMultiSet[T] = new MapMultiSet[T](countMap)

  // Convenience method
  def apply[T](items: Iterable[T]): MapMultiSet[T] = new MapMultiSet[T](items)

  // Convenience method
  def apply[T](args: T*): MapMultiSet[T] = new MapMultiSet[T](args)
}