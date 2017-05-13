package com.jgdodson.collections

import scala.collection.mutable

class DirectedGraph[T](directedConnections: Iterable[(T, T)]) extends AbstractGraph[T] {

  val nodes: Vector[T] = directedConnections.flatMap(edge => Set(edge._1, edge._2)).toVector

  // Remove all self-connections and duplicates
  val edges: Vector[(T, T)] = directedConnections.filter(conn => conn._1 != conn._2).toSet.toVector

  // Remove all duplicates
  val adjacencyMap: Map[T, Vector[T]] = {

    // Builder
    val b = mutable.Map[T, mutable.Set[T]]()

    for (node <- nodes) {
      b(node) = mutable.Set[T]()
    }

    for (edge <- edges) {
      b(edge._1).add(edge._2)
    }

    b.mapValues(_.toVector).toMap
  }


  def dfs(start: T, target: T): Int = {
    ???
  }

  def bfs(start: T, target: T): Int = {
    ???
  }
}

object DirectedGraph {

  def apply[T](directedConnections: Iterable[(T, T)]): DirectedGraph[T] = new DirectedGraph[T](directedConnections)

  /**
    * Create a DirectedGraph from an adjacency map
    *
    * @param adjMap
    * @tparam T
    * @return
    */
  def fromAdjacencyMap[T](adjMap: Map[T, Iterable[T]]): DirectedGraph[T] = {
    DirectedGraph(adjMap.flatMap(p => p._2.map(q => (p._1, q))))
  }

}