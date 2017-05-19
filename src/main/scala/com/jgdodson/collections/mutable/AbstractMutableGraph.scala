package com.jgdodson.collections.mutable

import com.jgdodson.collections.AbstractGraph

/**
  * An abstract base class for mutable graphs
  *
  * @tparam T
  */
abstract class AbstractMutableGraph[T] extends AbstractGraph[T] {

  def addNode(elem: T): Unit

  def addEdge(edge: (T, T)): Unit

}

object AbstractMutableGraph {

}