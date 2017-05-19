package com.jgdodson.collections


class LabeledGraph[T, U](connections: Iterable[(T, T)], labels: (T, T) => U) extends Graph[T](connections) {



}

object LabeledGraph {

}