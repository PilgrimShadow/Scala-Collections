package com.jgdodson.collections

class DirectedGraph[T](directedConnections: Iterable[(T, T)]) extends AbstractGraph[T] {

  val nodes: Vector[T] = directedConnections.flatMap(edge => Set(edge._1, edge._2)).toVector

  // Remove all self-connections and duplicates
  val edges: Set[(T, T)] = directedConnections.filter(conn => conn._1 != conn._2).toSet


  /**
    * Determine whether this graph contains an edge
    *
    * Part of the Set trait
    *
    * @param edge The edge to test for membership
    * @return
    */
  def contains(edge: (T, T)): Boolean = {
    edges.contains(edge)
  }


  /**
    * Add a new edge to the graph
    *
    * Part of the Set trait
    *
    * @param edge The edge to add
    * @return
    */
  def +(edge: (T, T)): DirectedGraph[T] = {
    DirectedGraph(edges + edge)
  }


  /**
    * Remove an edge from this graph
    *
    * Part of the Set trait
    *
    * @param edge The edge to remove
    * @return
    */
  def -(edge: (T, T)): DirectedGraph[T] = {
    DirectedGraph(edges.filterNot(_ == edge))
  }


  /**
    * Return an empty graph
    *
    * Part of the Set trait
    *
    * @return
    */
  override def empty: DirectedGraph[T] = DirectedGraph[T](Nil)

  // Remove all duplicates
  val adjacencyMap: Map[T, Vector[T]] = {

    // Builder
    val b = collection.mutable.Map[T, collection.mutable.Set[T]]()

    for (node <- nodes) {
      b(node) = collection.mutable.Set[T]()
    }

    for (edge <- edges) {
      b(edge._1).add(edge._2)
    }

    b.mapValues(_.toVector).toMap
  }
}


object DirectedGraph {

  def apply[T](directedConnections: Iterable[(T, T)]): DirectedGraph[T] = new DirectedGraph[T](directedConnections)

  /**
    * Create a DirectedGraph from an adjacency map
    *
    * TODO: Write an alternate constructor
    *
    * @param adjMap
    * @tparam T
    * @return
    */
  def fromAdjacencyMap[T](adjMap: Map[T, Iterable[T]]): DirectedGraph[T] = {
    DirectedGraph(adjMap.flatMap(p => p._2.map(q => (p._1, q))))
  }

}