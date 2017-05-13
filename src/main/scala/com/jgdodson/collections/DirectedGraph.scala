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


  /**
    * Search for a path between the start node and the target node
    *
    * @param start  The starting node
    * @param target The target node
    * @return
    */
  def dfs(start: T, target: T): Option[Vector[T]] = {

    // All the nodes we have visited while searching
    val visited = mutable.Set[T](start)

    // The path between the start node and our search position
    val path = mutable.Stack[T](start)

    // Trivial case
    if (start == target) {
      return Some(path.toVector)
    }

    // Anonymous helper function
    lazy val helper: (T => Unit) = (node: T) => {

      // For every neighboring node we have not visited
      for (n <- adjacencyMap(node).filterNot(visited.contains)) {

        // We have now visited this node
        visited.add(n)

        // Add n to our current path
        path.push(n)

        // If the target has been found, stop searching and return the path
        if (n == target) {
          return Some(path.reverse.toVector)
        }

        // Search the unexplored subtree rooted at n
        helper(n)

        // Remove n from our current path
        path.pop()
      }
    }

    // Begin searching from the start node
    helper(start)

    // No path to the target was found
    None
  }


  /**
    * Search for a path between the start node and the target node
    *
    * @param start  The start node
    * @param target The target node
    * @return
    */
  def bfs(start: T, target: T): Option[Vector[T]] = {

    // All the nodes we have visited while searching
    val visited = mutable.Set[T](start)

    // The roots of sub-trees left to visit
    val toVisit = mutable.Queue[Vector[T]](Vector(start))

    // Trivial case
    if (start == target) {
      return Some(Vector(start))
    }

    // While unexplored sub-trees remain...
    while (toVisit.nonEmpty) {

      // The next node to search
      val path = toVisit.dequeue()

      for (n <- adjacencyMap(path.last).filterNot(visited.contains)) {

        // If the target has been found, stop searching and return the path
        if (n == target) {
          return Some(path :+ n)
        }

        // We've now visited this node
        visited.add(n)

        // Later, we'll explore all the nodes adjacent to this one
        toVisit.enqueue(path :+ n)
      }

    }

    // No path to the target was found
    None
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