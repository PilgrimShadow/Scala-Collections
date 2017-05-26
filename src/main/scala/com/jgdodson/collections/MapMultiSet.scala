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
  // TODO: Research the proper Error to throw here
  assert(counts.values.forall(_ >= 0), s"MapMultiSet initialization failed: ${counts}")


  /**
    * Construct a MultiSet from an iterable collection of items
    *
    * Auxiliary constructor
    *
    * @param items The items to include in the MultiSet
    * @return
    */
  def this(items: TraversableOnce[T]) = this {

    val m = mutable.Map[T, Int]()

    for (item <- items) {
      m(item) = m.getOrElse(item, 0) + 1
    }

    m.toMap
  }

  /**
    *
    * @param elem
    * @return
    */
  def apply(elem: T): Int = countMap(elem)


  /**
    *
    * MapMultiSets are isomorphic to their backing maps, hence their hash codes are, too.
    *
    * @return
    */
  override def hashCode(): Int = countMap.hashCode()


  /**
    *
    * TODO: Research how to properly write this method
    *
    * @param obj
    * @return
    */
  override def equals(obj: Any): Boolean = obj match {
    case x: MapMultiSet[T] => x.countMap == this.countMap
    case _ => false
  }

  
  /**
    * Return a Map containing the counts of all elements
    *
    * @return
    */
  def countMap: Map[T, Int] = counts.filter(_._2 > 0)


  /**
    * The number of elements in the MultiSet
    */
  override val size: Int = countMap.values.sum


  /**
    * Return a MultiSet with the given element added
    *
    * @param elem The element to be added
    * @return
    */
  def +(elem: T): MapMultiSet[T] = {
    MapMultiSet[T](countMap.updated(elem, countMap.getOrElse(elem, 0) + 1))
  }


  /**
    * Create a MapMultiSet will all elements in the given collection added
    *
    * @param elems The elements to be added
    * @return
    */
  def ++(elems: Iterable[T]): MapMultiSet[T] = {

    val m = mutable.Map[T, Int]()

    for (elem <- elems) {
      m(elem) = m.getOrElse(elem, 0) + 1
    }

    new MapMultiSet(countMap.map(elem => (elem._1, elem._2 + m.getOrElse(elem._1, 0))))
  }


  /**
    * Return a MultiSet with the given element removed
    *
    * @param elem The element to remove
    * @return
    */
  def -(elem: T): MapMultiSet[T] = {
    if (countMap.contains(elem)) {
      if (countMap(elem) > 1) {
        MapMultiSet(countMap.updated(elem, countMap(elem) - 1))
      } else {
        MapMultiSet(countMap - elem)
      }
    } else {
      this
    }
  }


  /**
    * Create a MapMultiSet will all elements in the given collection removed
    *
    * @param elems The elements to be removed
    * @return
    */
  def --(elems: Iterable[T]): MapMultiSet[T] = {

    val m = mutable.Map[T, Int]()

    for (elem <- elems) {
      m(elem) = m.getOrElse(elem, 0) + 1
    }

    new MapMultiSet(countMap.map(elem => (elem._1, math.max(0, elem._2 - m.getOrElse(elem._1, 0)))))
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
    * Determine whether this MapMultiSet is empty
    *
    * @return
    */
  override def isEmpty: Boolean = {
    countMap.isEmpty
  }

  /**
    * Determine whether this MapMultiSet is non-empty
    *
    * @return
    */
  override def nonEmpty: Boolean = {
    countMap.nonEmpty
  }


  /**
    * Return a new MapMultiSet where elements have been transformed with the given function
    *
    * @param f The function to map over the elements
    * @tparam B The type of the resulting elements
    * @return
    */
  def map[B](f: (T) => B): MapMultiSet[B] = {
    new MapMultiSet(countMap.map(p => (f(p._1), p._2)))
  }


  /**
    * Sum the elements of this MultiSet, assuming they are Numeric
    *
    * @param num
    * @tparam B The type of the resultant sum
    * @return
    */
  override def sum[B >: T](implicit num: Numeric[B]): B = countMap.foldLeft(num.zero)((acc, next) => num.plus(acc, num.times(next._1, num.fromInt(next._2))))


  /**
    * Return the maximum element in the MultiSet
    *
    * @param cmp
    * @tparam B The type of the resultant max
    * @return
    */
  override def max[B >: T](implicit cmp: Ordering[B]): T = countMap.keysIterator.max[B]


  /**
    * Return the maximum element, after applying the given transformation
    *
    * @param f A function to apply to elements before comparing them
    * @param cmp
    * @tparam B The type of the resultant max
    * @return
    */
  override def maxBy[B](f: (T) => B)(implicit cmp: Ordering[B]): T = countMap.keysIterator.maxBy[B](f)


  /**
    * Return the minimum element in the MultiSet
    *
    * @param cmp
    * @tparam B The type of the resultant min
    * @return
    */
  override def min[B >: T](implicit cmp: Ordering[B]): T = countMap.keysIterator.min[B]


  /**
    * Return the minimum element, after applying the given transformation
    *
    * @param f A function to apply to elements before comparing them
    * @param cmp
    * @tparam B The type of the resultant min
    * @return
    */
  override def minBy[B](f: (T) => B)(implicit cmp: Ordering[B]): T = countMap.keysIterator.minBy[B](f)


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
    * Keep only elements that satisfy a predicate
    *
    * @param p The predicate used to filter elements
    * @return
    */
  override def filter(p: (T) => Boolean): MapMultiSet[T] = {

    val m = countMap.filterKeys(p)

    // Only create a new MultiSet if elements were removed
    if (m.size == countMap.size) {
      this
    } else {
      MapMultiSet(m)
    }
  }


  /**
    * Keep only elements that DO NOT satisfy a predicate
    *
    * @param p The predicate used to filter elements
    * @return
    */
  override def filterNot(p: (T) => Boolean): MapMultiSet[T] = {

    val m = countMap.filterKeys(!p(_))

    // Only create a new MultiSet if elements were removed
    if (m.size == countMap.size) {
      this
    } else {
      MapMultiSet(m)
    }
  }


  /**
    * Find the first element satisfying the given collection
    *
    * @param p The predicate used to test elements
    * @return
    */
  override def find(p: (T) => Boolean): Option[T] = {
    countMap.keysIterator.find(p)
  }


  /**
    * Count the number of elements which satisfy a given predicate
    *
    * @param p The predicate used to test elements
    * @return
    */
  override def count(p: T => Boolean): Int = {
    countMap.foldLeft[Int](0)((acc, next) => acc + (if (p(next._1)) next._2 else 0))
  }

  /**
    * Tests whether a predicate holds for at least one element of the MapMultiSet
    *
    * @param p The predicate used to test elements
    * @return
    */
  override def exists(p: T => Boolean): Boolean = {
    countMap.keysIterator.exists(p)
  }


  /**
    * Create a set using the elements from this MultiSet
    *
    * @return
    */
  override def toSet[B >: T]: Set[B] = countMap.keysIterator.asInstanceOf[Iterator[B]].toSet


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
  def iterator: Iterator[T] = countMap.iterator.flatMap(p => Iterator.fill(p._2)(p._1))
}

object MapMultiSet {

  // Convenience method
  def apply[T](countMap: Map[T, Int]): MapMultiSet[T] = new MapMultiSet[T](countMap)

  // Convenience method
  def apply[T](items: TraversableOnce[T]): MapMultiSet[T] = new MapMultiSet[T](items)

  // Convenience method
  def apply[T](args: T*): MapMultiSet[T] = new MapMultiSet[T](args)
}