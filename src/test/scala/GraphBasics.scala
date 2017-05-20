import org.scalacheck.{Properties, Arbitrary}
import org.scalacheck.Prop.forAll

import com.jgdodson.collections.Graph

object GraphBasics extends Properties("Graph Basics") {

  val nonEqualPairs = Arbitrary.arbitrary[(Int, Int)] suchThat (pair => pair._1 != pair._2)

  property("Graph.contains is symmetric") = forAll(nonEqualPairs) { (p) =>
    val g = Graph(List(p))
    g.contains(p) && g.contains((p._2, p._1))
  }


}
