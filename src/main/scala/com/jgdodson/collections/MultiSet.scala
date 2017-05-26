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
    * Return a MultiSet with the given element added
    *
    * @param elem The element to be added
    * @return
    */
  def +(elem: T): MultiSet[T]


  /**
    * Return a MultiSet with the given element removed
    *
    * @param elem The element to remove
    * @return
    */
  def -(elem: T): MultiSet[T]

  // TODO: Research what type the parameter should be
  def ++(elems: Iterable[T]): MultiSet[T]

  // TODO: Research what type the parameter should be
  def --(elems: Iterable[T]): MultiSet[T]

  /**
    * Determine whether the MultiSet contains an item
    *
    * @param elem The element to test for membership
    * @return
    */
  def contains(elem: T): Boolean


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
    * Return the String representation of this MultiSet
    *
    * @return
    */
  override def toString: String = s"MultiSet(${iterator.mkString(", ")})"

}

object MultiSet {

  // Convenience method
  def apply[T](countMap: Map[T, Int]): MultiSet[T] = new MapMultiSet[T](countMap)

  // Convenience method
  def apply[T](items: Iterable[T]): MultiSet[T] = new MapMultiSet[T](items)

  // Convenience method
  def apply[T](args: T*): MultiSet[T] = new MapMultiSet[T](args)

}