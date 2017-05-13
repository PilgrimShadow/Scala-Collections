package com.jgdodson.collections


abstract class AbstractGraph[T] {

  def nodes: Iterable[T]

  def edges: Iterable[(T, T)]

  def adjacencyMap: Map[T, Iterable[T]]
}
