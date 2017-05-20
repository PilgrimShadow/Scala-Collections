package com.jgdodson.collections


abstract class AbstractGraph[T] extends Set[(T, T)] {

  def nodes: Iterable[T]

  def edges: Iterable[(T, T)]

  def adjacencyMap: Map[T, Iterable[T]]


  /**
    * Return an Iterator over the edges of the graph
    *
    * Part of the Set trait
    *
    * @return
    */
  def iterator: Iterator[(T, T)] = edges.iterator


  /**
    * Search for a path between the start node and the target node
    *
    * @param start  The starting node
    * @param target The target node
    * @return
    */
  def dfs(start: T, target: T): Vector[T] = {

    // Trivial case
    if (start == target) {
      return Vector(start)
    }

    // All the nodes we have visited while searching
    val visited = collection.mutable.Set[T](start)

    // The path between the start node and our search position
    val path = collection.mutable.Stack[T](start)

    // Helper function that searches the subtree rooted at `node`
    lazy val helper: (T => Unit) = (node: T) => {

      // For every neighboring node we have not visited
      for (n <- adjacencyMap(node).filterNot(visited.contains)) {

        // We have now visited this node
        visited.add(n)

        // Add n to our current path
        path.push(n)

        // If the target has been found, stop searching and return the path
        if (n == target) {
          return path.reverse.toVector
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
    Vector()
  }


  /**
    * Search for a path between the start node and the target node
    *
    * @param start  The start node
    * @param target The target node
    * @return
    */
  def bfs(start: T, target: T): Vector[T] = {

    // All the nodes we have visited while searching
    val visited = collection.mutable.Set[T](start)

    // The roots of sub-trees left to visit
    val toVisit = collection.mutable.Queue[Vector[T]](Vector(start))

    // Trivial case
    if (start == target) {
      return Vector(start)
    }

    // While unexplored sub-trees remain...
    while (toVisit.nonEmpty) {

      // The next node to search
      val path = toVisit.dequeue()

      for (n <- adjacencyMap(path.last).filterNot(visited.contains)) {

        // If the target has been found, stop searching and return the path
        if (n == target) {
          return path :+ n
        }

        // We've now visited this node
        visited.add(n)

        // Later, we'll explore all the nodes adjacent to this one
        toVisit.enqueue(path :+ n)
      }

    }

    // No path to the target was found
    Vector()
  }
}
