package funsets

/**
 * This class is a test suite for the methods in object FunSets.
 *
 * To run this test suite, start "sbt" then run the "test" command.
 */
class FunSetSuite extends munit.FunSuite:

  import FunSets.*

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets:
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val under10: FunSet = x => x < 10
    val above5: FunSet = x => x > 5

  /**
   * This test is currently disabled (by using .ignore) because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", remove the
   * .ignore annotation.
   */

  test("singleton set one contains one") {
    
    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets:
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
  }

  test("union contains all elements of each set") {
    new TestSets:
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
  }

  test("intersect contains all overlapping elements") {
    new TestSets:
      val s = intersect(under10, above5)
      assert(!contains(s, 1), "1 is not above 5")
      assert(!contains(s, 2), "2 is not above 5")
      assert(!contains(s, 3), "3 is not above 5")
      assert(!contains(s, 4), "4 is not above 5")
      assert(!contains(s, 5), "5 is not above 5")
      assert(contains(s, 6), "6 is under 10 and above 5")
      assert(contains(s, 7), "7 is under 10 and above 5")
      assert(contains(s, 8), "8 is under 10 and above 5")
      assert(contains(s, 9), "9 is under 10 and above 5")
      assert(!contains(s, 10), "10 is not under 10")
      assert(!contains(s, 11), "11 is not under 10")
  }

  test("diff contains all non overlapping elements") {
    new TestSets:
      val s = diff(under10, above5)
      assert(contains(s, 1), "1 is not above 5")
      assert(contains(s, 2), "2 is not above 5")
      assert(contains(s, 3), "3 is not above 5")
      assert(contains(s, 4), "4 is not above 5")
      assert(contains(s, 5), "5 is not above 5")
      assert(!contains(s, 6), "6 is under 10 and above 5")
      assert(!contains(s, 7), "7 is under 10 and above 5")
      assert(!contains(s, 8), "8 is under 10 and above 5")
      assert(!contains(s, 9), "9 is under 10 and above 5")
      assert(!contains(s, 10), "10 is not under 10")
      assert(!contains(s, 11), "11 is not under 10")
  }

  test("filter contains all elements that in given criteria") {
    new TestSets:
      val s = filter(above5, x => x % 3 == 0) // multiple of 3
      assert(!contains(s, 1), "1 is not above 5")
      assert(!contains(s, 3), "3 is not above 5")
      assert(!contains(s, 5), "5 is not above 5")
      assert(contains(s, 6), "6 is above 5 and multiple of 3")
      assert(!contains(s, 7), "7 is above 5 but not multiple of 3")
      assert(!contains(s, 8), "8 is above 5 but not multiple of 3")
      assert(contains(s, 9), "9 is above 5 and multiple of 3")
  }

  import scala.concurrent.duration.*
  override val munitTimeout = 10.seconds
