package com.jgdodson.collections

import scala.collection.mutable


class Graph[T](connections: Iterable[(T, T)]) extends AbstractGraph[T] {

  val nodes: Vector[T] = connections.flatMap(edge => Set(edge._1, edge._2)).toVector

  // Remove all self-connections and duplicates
  val edges: Vector[(T, T)] = connections.filter(conn => conn._1 != conn._2).map(edge => Set(edge._1, edge._2)).toSet.toVector.map(
    (p: Set[T]) => {
      val v = p.toVector
      (v(0), v(1))
    })

  val adjacencyMap: Map[T, Vector[T]] = {

    // Builder
    val b = mutable.Map[T, mutable.Set[T]]()

    for (node <- nodes) {
      b(node) = mutable.Set[T]()
    }

    for (edge <- edges) {
      b(edge._1).add(edge._2)
      b(edge._2).add(edge._1)
    }

    b.mapValues(_.toVector).toMap
  }

  def degreeMap: Map[T, Int] = {
    ???
  }
}

object Graph {

  def apply[T](edgeList: Iterable[(T, T)]): Graph[T] = new Graph(edgeList)

}
