package com.jgdodson.collections


class Graph[T](connections: Iterable[(T, T)]) extends AbstractGraph[T] {

  val nodes: Vector[T] = connections.flatMap(edge => Set(edge._1, edge._2)).toVector

  // Remove all self-connections and duplicates
  val edges: Set[(T, T)] = connections.filter(conn => conn._1 != conn._2).map(edge => Set(edge._1, edge._2)).toSet.map(
    (p: Set[T]) => {
      val v = p.toVector
      (v(0), v(1))
    })

  // Part of the Set trait
  def contains(edge: (T, T)): Boolean = {
    edges.contains(edge)
  }

  // Part of the Set trait
  def +(elem: (T, T)): Graph[T] = {
    Graph(edges + elem)
  }

  // Part of the Set trait
  def -(elem: (T, T)): Graph[T] = {
    Graph(edges.filterNot(edge => edge == elem))
  }

  // Part of the Set trait
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

  def apply[T](edgeList: Iterable[(T, T)]): Graph[T] = new Graph(edgeList)
}
