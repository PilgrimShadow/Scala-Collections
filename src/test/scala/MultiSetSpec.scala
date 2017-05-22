import org.scalacheck.{Properties, Arbitrary, Gen}
import org.scalacheck.Prop.forAll

// The class being tested
import com.jgdodson.collections.MultiSet

object MultiSetSpec extends Properties("MultiSet Specification") {

  implicit def ArbitraryMultiSet[T](implicit arbitrary: Arbitrary[T]): Arbitrary[MultiSet[T]] = Arbitrary {

    Arbitrary.arbitrary[Map[T, Int]].map(m => MultiSet(m.mapValues(count => (if (count == 0 || count == Int.MinValue) 1 else math.abs(count)).min(100))))
  }

  def NonEmptyMultiSets[T](implicit arbitrary: Arbitrary[T]): Gen[MultiSet[T]] = ArbitraryMultiSet[T].arbitrary.suchThat(_.nonEmpty)


  property("Empty MultiSet is empty [Int]") = MultiSet[Int]().isEmpty

  property("Empty MultiSet is empty [String]") = MultiSet[String]().isEmpty

  property("Non-empty MultiSet is non-empty [Int]") = forAll(NonEmptyMultiSets[Int])(multi => multi.nonEmpty)

  property("Non-empty MultiSet is non-empty [String]") = forAll(NonEmptyMultiSets[String])(multi => multi.nonEmpty)

  property("Never simultaneously Empty and Non-Empty [Int]") = forAll((multi: MultiSet[Int]) => multi.nonEmpty != multi.isEmpty)

  property("Never simultaneously Empty and Non-Empty [String]") = forAll((multi: MultiSet[String]) => multi.nonEmpty != multi.isEmpty)

  property("Size is non-negative [Int]") = forAll((multi: MultiSet[Int]) => multi.size >= 0)

  property("Size is non-negative [String]") = forAll((multi: MultiSet[String]) => multi.size >= 0)

  property("Adding item increases size by 1 [Int]") = forAll((multi: MultiSet[Int], item: Int) => (multi + item).size == multi.size + 1)

  property("Adding item to empty MultiSet [Int]") = forAll((item: Int) => {

    val e = MultiSet[Int]()

    val s = e + item

    s.contains(item)
    s.count(item) == 1
  })


  property("Adding item to empty MultiSet [String]") = forAll((item: String) => {

    val e = MultiSet[String]()

    val s = e + item

    s.contains(item)
    s.count(item) == 1
  })


  property("MultiSet mode has highest count [Int]") = forAll(NonEmptyMultiSets[Int])(multi => {

    multi.countMap.values.forall(_ <= multi.mode._2)
  })

  property("MultiSet mode has highest count [String]") = forAll(NonEmptyMultiSets[String])(multi => {

    multi.countMap.values.forall(_ <= multi.mode._2)
  })
}
