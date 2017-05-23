package com.jgdodson.collections.mutables

// Standard Library
import scala.collection.mutable

// Project
import com.jgdodson.collections.MultiSet

/**
  *
  * @tparam T The type of element in the resulting MultiSet
  */
class MultiSetBuilder[T] extends mutable.ReusableBuilder[T, MultiSet[T]] {

  private val builderMap = mutable.Map[T, Int]()

  def +=(elem: T): MultiSetBuilder.this.type = {

    builderMap.update(elem, builderMap.getOrElse(elem, 0) + 1)

    this
  }

  def clear(): Unit = builderMap.clear()

  def result(): MultiSet[T] = MultiSet(builderMap.toMap)

}
