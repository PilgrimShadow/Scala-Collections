
// ScalaCheck
import org.scalacheck.{Properties, Arbitrary, Gen}
import org.scalacheck.Prop
import org.scalacheck.Prop.forAll

// The class being tested
import com.jgdodson.collections.MapMultiSet

object MapMultiSetSpec extends Properties("MapMultiSet Specification") {

  /**
    * Generate arbitrary MultiSets
    *
    * TODO: Add a parameter to indicate elements which should be excluded
    *
    * @param arb
    * @tparam T The type of item the arbitrary MultiSets will contain
    * @return
    */
  implicit def ArbitraryMultiSet[T](implicit arb: Arbitrary[T]): Arbitrary[MapMultiSet[T]] = Arbitrary {

    // Beware Int.MaxValue and Int.MinValue
    // We cap the count for a single element at 100 to prevent heap overflow from large test values
    Arbitrary.arbitrary[Map[T, Int]].map(m => MapMultiSet(m.mapValues(count => (if (count == 0 || count == Int.MinValue) 1 else math.abs(count)).min(100))))
  }

  /**
    * Generate arbitrary, non-empty MultiSets
    *
    * @param arb
    * @tparam T The type of item the arbitrary MultiSets will contain
    * @return
    */
  def NonEmptyMultiSets[T](implicit arb: Arbitrary[T]): Gen[MapMultiSet[T]] = ArbitraryMultiSet[T].arbitrary.suchThat(_.nonEmpty)


  /**
    * Test that an empty MultiSet always reports as being empty
    *
    * @tparam T The type of item a tested MultiSet will contain
    * @return
    */
  def EmptyMultiSetIsEmpty[T]: Boolean = MapMultiSet[T]().isEmpty


  /**
    * Test that the emptiness of a MultiSet always agrees with its size
    *
    * @param arb
    * @tparam T The type of item a tested MultiSet will contain
    * @return
    */
  def EmptinessAndSizeAgree[T](implicit arb: Arbitrary[T]): Prop = forAll((multi: MapMultiSet[T]) => {
    (multi.isEmpty && multi.size == 0) || (multi.nonEmpty && multi.size > 0)
  })


  /**
    * Test that the size of a MultiSet is always non-negative
    *
    * @param arb
    * @tparam T The type of item a tested MultiSet will contain
    * @return
    */
  def SizeIsNonNegative[T](implicit arb: Arbitrary[T]): Prop = forAll((multi: MapMultiSet[T]) => multi.size >= 0)


  /**
    * Test that a MultiSet is never simultaneously empty and non-empty
    *
    * @param arb
    * @tparam T The type of item a tested MultiSet will contain
    * @return
    */
  def NeverEmptyAndNonEmpty[T](implicit arb: Arbitrary[T]): Prop = forAll((multi: MapMultiSet[T]) => multi.nonEmpty != multi.isEmpty)


  /**
    * Test that adding an item to a MultiSet always increases its size by 1
    *
    * @param arb
    * @tparam T The type of item a tested MultiSet will contain
    * @return
    */
  def AddingItemIncrementsSize[T](implicit arb: Arbitrary[T]): Prop = forAll((multi: MapMultiSet[Int], item: Int) => (multi + item).size == multi.size + 1)


  /**
    * Test that the mode of a MultiSet is always the most numerous element
    *
    * @param arb
    * @tparam T The type of item a tested MultiSet will contain
    * @return
    */
  def ModeHasHighestCount[T](implicit arb: Arbitrary[T]): Prop = forAll(NonEmptyMultiSets[String])(multi => {

    multi.countMap.values.forall(_ <= multi.mode._2)
  })


  /**
    * Test that the max of a MultiSet is the max of its Iterator
    *
    * @param arb
    * @param ord
    * @tparam T The type of item a tested MultiSet will contain
    * @return
    */
  def MaxEqualsMaxOfIterator[T](implicit arb: Arbitrary[T], ord: Ordering[T]): Prop = forAll(NonEmptyMultiSets[T])(multi => {

    multi.max == multi.iterator.max
  })


  /**
    * Test that the min of a MultiSet equals the min of its Iterator
    *
    * @param arb
    * @param ord
    * @tparam T The type of item a tested MultiSet will contain
    * @return
    */
  def MinEqualsMinOfIterator[T](implicit arb: Arbitrary[T], ord: Ordering[T]): Prop = forAll(NonEmptyMultiSets[T])(multi => {

    multi.min == multi.iterator.min
  })


  /**
    * Test that the sum of a MultiSet equals the sum of its Iterator
    *
    * @param arb
    * @param num
    * @tparam T The type of item a tested MultiSet will contain
    * @return
    */
  def SumEqualsSumOfIterator[T](implicit arb: Arbitrary[T], num: Numeric[T]): Prop = forAll((multi: MapMultiSet[T]) => {

    multi.iterator.sum == multi.sum
  })

  /**
    * Test that all elements with zero-count are filtered out during initialization
    *
    * @param arb
    * @tparam T
    * @return
    */
  def ZeroCountsAreFilteredOut[T](implicit arb: Arbitrary[T]): Prop = forAll((elem: T) => {
    val multi = MapMultiSet[T](Map(elem -> 0))

    !multi.countMap.contains(elem)
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

  property("MultiSet mode has highest count [Int]") = ModeHasHighestCount[Int]

  property("MultiSet mode has highest count [BigInt]") = ModeHasHighestCount[BigInt]

  property("MultiSet mode has highest count [String]") = ModeHasHighestCount[String]

  property("Sum of MultiSet Equals Sum of its Iterator [Int]") = SumEqualsSumOfIterator[Int]

  property("Sum of MultiSet Equals Sum of its Iterator [Short]") = SumEqualsSumOfIterator[Short]

  property("Sum of MultiSet Equals Sum of its Iterator [BigInt]") = SumEqualsSumOfIterator[BigInt]

  property("Max of MultiSet Equals Max of its Iterator [Int]") = MaxEqualsMaxOfIterator[Int]

  property("Max of MultiSet Equals Max of its Iterator [Short]") = MaxEqualsMaxOfIterator[Short]

  property("Max of MultiSet Equals Max of its Iterator [BigInt]") = MaxEqualsMaxOfIterator[BigInt]

  property("Min of MultiSet Equals Min of its Iterator [Int]") = MinEqualsMinOfIterator[Int]

  property("Min of MultiSet Equals Min of its Iterator [Short]") = MinEqualsMinOfIterator[Short]

  property("Min of MultiSet Equals Min of its Iterator [BigInt]") = MinEqualsMinOfIterator[BigInt]

  property("Zero-count elements are filtered out [Int]") = ZeroCountsAreFilteredOut[Int]

  property("Zero-count elements are filtered out [Short]") = ZeroCountsAreFilteredOut[Short]

  property("Zero-count elements are filtered out [BigInt]") = ZeroCountsAreFilteredOut[BigInt]
}
