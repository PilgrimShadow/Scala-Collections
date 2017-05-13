package com.jgdodson.collections

import scala.collection.mutable


abstract class AbstractGraph[T] {

  def nodes: Iterable[T]

  def edges: Iterable[(T, T)]

  def adjacencyMap: Map[T, Iterable[T]]


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
