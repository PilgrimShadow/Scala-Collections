package com.jgdodson.collections


class Graph[T](connections: Iterable[(T, T)]) extends AbstractGraph[T] {

  val nodes: Vector[T] = connections.flatMap(edge => Set(edge._1, edge._2)).toVector

  // Remove all self-connections and duplicates
  val edges: Set[(T, T)] = connections.filter(conn => conn._1 != conn._2).map(edge => Set(edge._1, edge._2)).toSet.map(
    (p: Set[T]) => {
      val v = p.toVector
      (v(0), v(1))
    })


  /**
    * Determine whether this graph contains an edge
    *
    * Part of the Set trait
    *
    * @param edge The edge to test for membership
    * @return
    */
  def contains(edge: (T, T)): Boolean = {
    edges.contains(edge) || edges.contains((edge._2, edge._1))
  }


  /**
    * Add a new edge to the graph
    *
    * Part of the Set trait
    *
    * @param edge The edge to add
    * @return
    */
  def +(edge: (T, T)): Graph[T] = {
    if (this.contains(edge)) {
      this
    } else {
      Graph(edges + edge)
    }
  }


  /**
    * Remove an edge from this graph
    *
    * Part of the Set trait
    *
    * @param edge The edge to remove
    * @return
    */
  def -(edge: (T, T)): Graph[T] = {
    val rev = (edge._2, edge._1)
    Graph(edges.filterNot(existing => (existing == edge) || (existing == rev)))
  }


  /**
    * Return an empty graph
    *
    * Part of the Set trait
    *
    * @return
    */
  override def empty: Graph[T] = Graph[T](Nil)


  val adjacencyMap: Map[T, Vector[T]] = {

    // Builder
    val b = collection.mutable.Map[T, collection.mutable.Set[T]]()

    for (node <- nodes) {
      b(node) = collection.mutable.Set[T]()
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

  /**
    * Convenience method for instantiation without new
    *
    * @param edgeList
    * @tparam T
    * @return
    */
  def apply[T](edgeList: Iterable[(T, T)]): Graph[T] = new Graph(edgeList)


  /**
    * Create a Graph from a DirectedGraph
    *
    * @param dg
    * @tparam T
    * @return
    */
  def apply[T](dg: DirectedGraph[T]): Graph[T] = Graph(dg.edges)
}
