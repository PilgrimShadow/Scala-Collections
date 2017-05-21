import org.scalacheck.{Properties, Arbitrary, Gen}
import org.scalacheck.Prop.forAll

// The class being tested
import com.jgdodson.collections.MultiSet

object MultiSetSpec extends Properties("MultiSet Specification") {

  def MultiSets[T](implicit arbitrary: Arbitrary[T]): Gen[MultiSet[T]] = {

    // Beware Int.MinValue and Int.MaxValue
    Arbitrary.arbitrary[Map[T, Int]].map(m => MultiSet(m.mapValues(count => if (count == 0 || count == Int.MinValue) 1 else math.abs(count))))
  }

  def NonEmptyMultiSets[T](implicit arbitrary: Arbitrary[T]): Gen[MultiSet[T]] = MultiSets[T].suchThat(_.nonEmpty)


  property("Empty MultiSet is empty [Int]") = MultiSet[Int]().isEmpty

  property("Non-empty MultiSet is non-empty [Int]") = forAll(NonEmptyMultiSets[Int])((multi) => {

    !multi.isEmpty
  })

  property("Never simultaneously Empty and Non-Empty") = forAll(MultiSets[Int])((multi) => {
    multi.nonEmpty != multi.isEmpty
  })

  property("Adding item to empty MultiSet [Int]") = forAll((item: Int) => {

    val e = MultiSet[Int]()

    val s = e + item

    s.contains(item)
    s.count(item) == 1
  })


  property("MultiSet mode has highest count [Int]") = forAll(NonEmptyMultiSets[Int])((multi) => {

    multi.countMap.values.forall(_ <= multi.mode._2)
  })

}
