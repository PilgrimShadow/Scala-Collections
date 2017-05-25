package com.jgdodson.collections


/**
  * A traditional multiset, which can contain numerous copies of an element
  *
  * TODO: Decide what class MultiSet should extend
  *
  * @tparam T The type of item contained in the MultiSet
  */
trait MultiSet[T] extends Iterable[T] {


  /**
    * Return a Map containing the counts of all elements
    *
    * @return
    */
  def countMap: Map[T, Int]


  /**
    * Return the most commonly occurring item in the MultiSet
    *
    * If there is more than one mode, only one of them will be returned.
    *
    * @return
    */
  def mode: (T, Int)


  /**
    * Return the count of the given element
    *
    * @param elem The element for which to retrieve the count
    * @return
    */
  def count(elem: T): Int

}

object MultiSet {

  // Convenience method
  def apply[T](countMap: Map[T, Int]): MapMultiSet[T] = new MapMultiSet[T](countMap)

  // Convenience method
  def apply[T](items: Iterable[T]): MapMultiSet[T] = new MapMultiSet[T](items)

  // Convenience method
  def apply[T](args: T*): MapMultiSet[T] = new MapMultiSet[T](args)

}