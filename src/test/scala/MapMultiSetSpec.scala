
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
    * @param arb An object to generate arbitrary instances of T
    * @tparam T The type of item the arbitrary MultiSets will contain
    * @return
    */
  implicit def ArbitraryMultiSet[T](implicit arb: Arbitrary[T]): Arbitrary[MapMultiSet[T]] = Arbitrary {

    // Beware Int.MaxValue and Int.MinValue
    // We cap the count for a single element at 100 to prevent heap overflow from large test values
    Arbitrary.arbitrary[Map[T, Int]].map(m => MapMultiSet(m.mapValues(count => (if (count == 0 || count == Int.MinValue) 1 else math.abs(count)).min(100))))
  }


  /**
    * Generate arbitrary predicate functions
    *
    * Is this sufficient? Perhaps use the ScalaCheck RNG to vary the output
    *
    * @tparam T The input type of the generated predicates
    * @return
    */
  implicit def ArbitraryPredicate[T]: Arbitrary[T => Boolean] = Arbitrary {

    (t: T) => t.hashCode() % 2 == 0
  }


  /**
    * Generate arbitrary, non-empty MultiSets
    *
    * @param arb An object to generate arbitrary instances of T
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
    * @param arb An object to generate arbitrary instances of T
    * @tparam T The type of item a tested MultiSet will contain
    * @return
    */
  def EmptinessAndSizeAgree[T](implicit arb: Arbitrary[T]): Prop = forAll((multi: MapMultiSet[T]) => {
    (multi.isEmpty && multi.size == 0) || (multi.nonEmpty && multi.size > 0)
  })


  /**
    * Test that the size of a MultiSet is always non-negative
    *
    * @param arb An object to generate arbitrary instances of T
    * @tparam T The type of item a tested MultiSet will contain
    * @return
    */
  def SizeIsNonNegative[T](implicit arb: Arbitrary[T]): Prop = forAll((multi: MapMultiSet[T]) => multi.size >= 0)


  /**
    * Test that the size of the MapMultiSet is equal to the size of its Iterator
    *
    * @param arb An object to generate arbitrary instances of T
    * @tparam T The type of item a tested MultiSet will contain
    * @return
    */
  def SizeIsSizeOfIterator[T](implicit arb: Arbitrary[T]): Prop = forAll((multi: MapMultiSet[T]) => {
    multi.size == multi.iterator.size
  })


  /**
    * Test that a MultiSet is never simultaneously empty and non-empty
    *
    * @param arb An object to generate arbitrary instances of T
    * @tparam T The type of item a tested MultiSet will contain
    * @return
    */
  def NeverEmptyAndNonEmpty[T](implicit arb: Arbitrary[T]): Prop = forAll((multi: MapMultiSet[T]) => multi.nonEmpty != multi.isEmpty)


  /**
    * Test that adding an item to a MultiSet always increases its size by 1
    *
    * @param arb An object to generate arbitrary instances of T
    * @tparam T The type of item a tested MultiSet will contain
    * @return
    */
  def AddingItemIncrementsSize[T](implicit arb: Arbitrary[T]): Prop = forAll((multi: MapMultiSet[Int], item: Int) => (multi + item).size == multi.size + 1)


  /**
    * Test that the mode of a MultiSet is always the most numerous element
    *
    * @param arb An object to generate arbitrary instances of T
    * @tparam T The type of item a tested MultiSet will contain
    * @return
    */
  def ModeHasHighestCount[T](implicit arb: Arbitrary[T]): Prop = forAll(NonEmptyMultiSets[String])(multi => {

    multi.countMap.values.forall(_ <= multi.mode._2)
  })


  /**
    * Test that the max of a MultiSet is the max of its Iterator
    *
    * @param arb An object to generate arbitrary instances of T
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
    * @param arb An object to generate arbitrary instances of T
    * @param ord
    * @tparam T The type of item a tested MultiSet will contain
    * @return
    */
  def MinEqualsMinOfIterator[T](implicit arb: Arbitrary[T], ord: Ordering[T]): Prop = forAll(NonEmptyMultiSets[T])(multi => {

    multi.min == multi.iterator.min
  })


  /**
    * Test that the sum of a MapMultiSet equals the sum of its Iterator
    *
    * @param arb An object to generate arbitrary instances of T
    * @param num
    * @tparam T The type of item a tested MultiSet will contain
    * @return
    */
  def SumEqualsSumOfIterator[T](implicit arb: Arbitrary[T], num: Numeric[T]): Prop = forAll((multi: MapMultiSet[T]) => {

    multi.sum == multi.iterator.sum
  })


  /**
    *
    * @param arb
    * @tparam T
    * @return
    */
  def CountOfMultiSetEqualsCountOfIterator[T](implicit arb: Arbitrary[T]): Prop = forAll((multi: MapMultiSet[T], p: T => Boolean) => {


    multi.count(p) == multi.iterator.count(p)
  })


  /**
    *
    * @param arb
    * @tparam T
    * @return
    */
  def ExistsOfMultiSetEqualsExistsOfIterator[T](implicit arb: Arbitrary[T]): Prop = forAll((multi: MapMultiSet[T], p: T => Boolean) => {
    multi.exists(p) == multi.iterator.exists(p)
  })

  /**
    *
    * @param arb
    * @tparam T
    * @return
    */
  def FilterEqualsFilterOfIterator[T](implicit arb: Arbitrary[T]): Prop = forAll((multi: MapMultiSet[T], p: T => Boolean) => {
    multi.filter(p) == MapMultiSet[T](multi.iterator.filter(p))
  })


  /**
    *
    * @param arb
    * @tparam T
    * @return
    */
  def FilterNotEqualsFilterNotOfIterator[T](implicit arb: Arbitrary[T]): Prop = forAll((multi: MapMultiSet[T], p: T => Boolean) => {
    multi.filterNot(p) == MapMultiSet[T](multi.iterator.filterNot(p))
  })

  /**
    *
    * @param arb
    * @tparam T
    * @return
    */
  def FindEqualsFindOfIterator[T](implicit arb: Arbitrary[T]): Prop = forAll((multi: MapMultiSet[T], p: T => Boolean) => {
    multi.find(p) == multi.iterator.find(p)
  })


  /**
    * Test that all elements with zero-count are filtered out during initialization
    *
    * @param arb An object to generate arbitrary instances of T
    * @tparam T The type of item a tested MultiSet will contain
    * @return
    */
  def ZeroCountsAreFilteredOut[T](implicit arb: Arbitrary[T]): Prop = forAll((elem: T) => {

    val multi = MapMultiSet[T](Map(elem -> 0))

    !multi.countMap.contains(elem)
  })


  // TODO: Write a test for groupBy


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

  property("Size is size of Iterator [Int]") = SizeIsSizeOfIterator[Int]

  property("Size is size of Iterator [Short]") = SizeIsSizeOfIterator[Short]

  property("Size is size of Iterator [BigInt]") = SizeIsSizeOfIterator[BigInt]

  property("Size is size of Iterator [String]") = SizeIsSizeOfIterator[String]

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

  property("Count of MultiSet Equals Count of its Iterator [Int]") = CountOfMultiSetEqualsCountOfIterator[Int]

  property("Count of MultiSet Equals Count of its Iterator [Short]") = CountOfMultiSetEqualsCountOfIterator[Short]

  property("Count of MultiSet Equals Count of its Iterator [BigInt]") = CountOfMultiSetEqualsCountOfIterator[BigInt]

  property("Exists of MultiSet Equals Exists of its Iterator [Int]") = ExistsOfMultiSetEqualsExistsOfIterator[Int]

  property("Exists of MultiSet Equals Exists of its Iterator [Short]") = ExistsOfMultiSetEqualsExistsOfIterator[Short]

  property("Exists of MultiSet Equals Exists of its Iterator [BigInt]") = ExistsOfMultiSetEqualsExistsOfIterator[BigInt]

  property("Filter of MultiSet Equals Filter of its Iterator [Int]") = FilterEqualsFilterOfIterator[Int]

  property("Filter of MultiSet Equals Filter of its Iterator [Short]") = FilterEqualsFilterOfIterator[Short]

  property("Filter of MultiSet Equals Filter of its Iterator [BigInt]") = FilterEqualsFilterOfIterator[BigInt]

  property("FilterNot of MultiSet Equals FilterNot of its Iterator [Int]") = FilterNotEqualsFilterNotOfIterator[Int]

  property("FilterNot of MultiSet Equals FilterNot of its Iterator [Short]") = FilterNotEqualsFilterNotOfIterator[Short]

  property("FilterNot of MultiSet Equals FilterNot of its Iterator [BigInt]") = FilterEqualsFilterOfIterator[BigInt]

  property("Find of MultiSet Equals Find of its Iterator [Int]") = FindEqualsFindOfIterator[Int]

  property("Find of MultiSet Equals Find of its Iterator [Short]") = FindEqualsFindOfIterator[Short]

  property("Find of MultiSet Equals Find of its Iterator [BigInt]") = FindEqualsFindOfIterator[BigInt]
}
