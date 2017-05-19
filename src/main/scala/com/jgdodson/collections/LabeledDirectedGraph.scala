package com.jgdodson.collections


class LabeledDirectedGraph[T, U](connections: Iterable[(T, T)], labels: (T, T) => U) extends DirectedGraph[T](connections) {



}

object LabeledDirectedGraph {

}