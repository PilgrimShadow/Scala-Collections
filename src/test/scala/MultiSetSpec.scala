import org.scalacheck.{Properties, Arbitrary, Gen}
import org.scalacheck.Prop
import org.scalacheck.Prop.forAll

// The class being tested
import com.jgdodson.collections.MultiSet

object MultiSetSpec extends Properties("MultiSet Specification") {

  /**
    * Generate arbitrary MultiSets
    *
    * TODO: Add a parameter to indicate elements which should be excluded
    *
    * @param arb
    * @tparam T
    * @return
    */
  implicit def ArbitraryMultiSet[T](implicit arb: Arbitrary[T]): Arbitrary[MultiSet[T]] = Arbitrary {

    // Beware Int.MaxValue and Int.MinValue
    // We cap the count for a single element at 100 to prevent heap overflow from large test values
    Arbitrary.arbitrary[Map[T, Int]].map(m => MultiSet(m.mapValues(count => (if (count == 0 || count == Int.MinValue) 1 else math.abs(count)).min(100))))
  }

  /**
    * Generate arbitrary, non-empty MultiSets
    *
    * @param arb
    * @tparam T
    * @return
    */
  def NonEmptyMultiSets[T](implicit arb: Arbitrary[T]): Gen[MultiSet[T]] = ArbitraryMultiSet[T].arbitrary.suchThat(_.nonEmpty)


  /**
    *
    * @tparam T The type of item a tested MultiSet will contain
    * @return
    */
  def EmptyMultiSetIsEmpty[T]: Boolean = MultiSet[T]().isEmpty


  /**
    * Test that the emptiness of a MultiSet always agrees with its size
    *
    * @param arb
    * @tparam T The type of item a tested MultiSet will contain
    * @return
    */
  def EmptinessAndSizeAgree[T](implicit arb: Arbitrary[T]): Prop = forAll((multi: MultiSet[T]) => {
    (multi.isEmpty && multi.size == 0) || (multi.nonEmpty && multi.size > 0)
  })


  /**
    * Test that the size of a MultiSet is always  non-negative
    *
    * @param arb
    * @tparam T The type of item a tested MultiSet will contain
    * @return
    */
  def SizeIsNonNegative[T](implicit arb: Arbitrary[T]): Prop = forAll((multi: MultiSet[T]) => multi.size >= 0)


  /**
    * Test that a MultiSet is never simultaneously empty and non-empty
    *
    * @param arb
    * @tparam T The type of item a tested MultiSet will contain
    * @return
    */
  def NeverEmptyAndNonEmpty[T](implicit arb: Arbitrary[T]): Prop = forAll((multi: MultiSet[T]) => multi.nonEmpty != multi.isEmpty)


  /**
    * Test that adding an item to a MultiSet always increases its size by 1
    *
    * @param arb
    * @tparam T The type of item a tested MultiSet will contain
    * @return
    */
  def AddingItemIncrementsSize[T](implicit arb: Arbitrary[T]): Prop = forAll((multi: MultiSet[Int], item: Int) => (multi + item).size == multi.size + 1)
  

  /**
    * Test that the mode of a MultiSet is always the most numerous element
    *
    * @param arb
    * @tparam T The type of item a tested MultiSet will contain
    * @return
    */
  def MultiSetModeHasHighestCount[T](implicit arb: Arbitrary[T]): Prop = forAll(NonEmptyMultiSets[String])(multi => {

    multi.countMap.values.forall(_ <= multi.mode._2)
  })

  property("Empty MultiSet is empty [Int]") = EmptyMultiSetIsEmpty[Int]

  property("Empty MultiSet is empty [BigInt]") = EmptyMultiSetIsEmpty[BigInt]

  property("Empty MultiSet is empty [String]") = EmptyMultiSetIsEmpty[String]

  property("Non-empty MultiSet is non-empty [Int]") = forAll(NonEmptyMultiSets[Int])(multi => multi.nonEmpty)

  property("Non-empty MultiSet is non-empty [String]") = forAll(NonEmptyMultiSets[String])(multi => multi.nonEmpty)

  property("Never simultaneously Empty and Non-Empty [Int]") = NeverEmptyAndNonEmpty[Int]

  property("Never simultaneously Empty and Non-Empty [BigInt]") = NeverEmptyAndNonEmpty[BigInt]

  property("Never simultaneously Empty and Non-Empty [String]") = NeverEmptyAndNonEmpty[String]

  property("Size is non-negative [Int]") = SizeIsNonNegative[Int]

  property("Size is non-negative [BigInt]") = SizeIsNonNegative[BigInt]

  property("Size is non-negative [String]") = SizeIsNonNegative[String]

  property("Adding item increases size by 1 [Int]") = AddingItemIncrementsSize[Int]

  property("Adding item increases size by 1 [BigInt]") = AddingItemIncrementsSize[BigInt]

  property("Adding item increases size by 1 [String]") = AddingItemIncrementsSize[String]

  property("MultiSet mode has highest count [Int]") = MultiSetModeHasHighestCount[Int]

  property("MultiSet mode has highest count [BigInt]") = MultiSetModeHasHighestCount[BigInt]

  property("MultiSet mode has highest count [String]") = MultiSetModeHasHighestCount[String]
}
