package leetcode

import munit._
import scala.compiletime.ops.double
class StringProblemsSuite extends munit.FunSuite:

  test("2114. Maximum number of words in a sentence") {
    val testcases = List(
      (Array("please wait", "continue to fight", "continue to win"), 3),
      (Array("alice and bob love leetcode", "i think so too", "this is great thanks very much"), 6),
      (Array("hello world", "unspoken truth of humanity"), 4)
    )
    for testcase <- testcases do
      testcase match
        case (input, expected) => assert(StringProblems.maxNumberOfWords(input) == expected)
  }

  test("1108. Defanging IP address".fail):
    val testCases = List(
      ("1.1.1.1", "1[.]1[.]1[.]1"),
      ("255.100.50.0", "255[.]100[.]50[.]0")
    )
    for (input, expected) <- testCases do assertEquals(StringProblems.defangingIPAddress(input), expected)

  test("709. To Lowercase"):
    val testcases = List(
      ("Must BE LOweR", "Must be lower")
    )
    for (input, e) <- testcases do assertEquals(StringProblems.toLowerCase(input), e)

  test("2129. Capitalize the title".ignore):
    val testcases = List(
      ("This is a Valid Title", "This is a Valid Title"),
      ("capiTalIze tHe titLe", "Capitalize The Title"),
      ("First leTTeR of EACH Word", "First Letter of Each Word"),
      ("i lOve leetcode", "i Love Leetcode")
    )
    for (input, expected) <- testcases do assertEquals(StringProblems.capitalizeTheTitle(input), expected)

  test("520. Detect capital"):
    val testcases = List(
      ("INDIA", true),
      ("india", true),
      ("India", true),
      ("InDIa", false)
    )
    for (input, expected) <- testcases do assertEquals(StringProblems.detectCapital(input), expected)

  test("771. Jewels and stones"):
    val testcases = List(
      (("aA", "aAAbbbb"), 3),
      (("z", "ZZ"), 0)
    )
    for ((jewels, stones), expected) <- testcases do
      assertEquals(
        StringProblems.jewelsAndStones(jewels, stones),
        expected,
        clue = s"Jewels: '$jewels' && stones:'$stones' must return '$expected'"
      )

  test("2011. Final value after performing operations"):
    val testcases = List(
      (Array("--X", "X++", "X++"), 1),
      (Array("++X", "++X", "X++"), 3),
      (Array("X++", "++X", "--X", "X--"), 0)
    )
    for (input, expected) <- testcases do
      assertEquals(
        StringProblems.finalValueAfterOps(input),
        expected,
        clue = s"Operations: '${input.mkString(",")}' must return '$expected'"
      )

  test("2942. Find words containing"):
    val testcases = List(
      ((Array("leet", "code"), 'e'), Array(0, 1)),
      ((Array("abc", "bcd", "aaaa", "cbc"), 'a'), Array(0, 2)),
      ((Array("abc", "bcd", "aaaa", "cbc"), 'z'), Array.empty[Int])
    )
    for ((words, x), expected) <- testcases do
      assert(
        StringProblems.findWordsContaining(words, x).sameElements(expected),
        clue = s"Expecting '${words.mkString(",")}' to contain '$x' in indices '${expected.mkString(",")}'"
      )

  test("1662. Check if two strings are equivalent"):
    val testcases = List(
      ((Array("ab", "c"), Array("a", "bc")), true),
      ((Array("a", "cb"), Array("ab", "c")), false),
      ((Array("abc", "d", "defg"), Array("abcddefg")), true)
    )
    for ((word1, word2), expected) <- testcases do
      assertEquals(
        StringProblems.arrayStringsAreEqual(word1, word2),
        expected,
        clue =
          s"Expecting equivalence between word1:'${word1.mkString(",")}' & word2:'${word2.mkString(",")}' as '$expected'"
      )

  test("1816. Truncate sentence"):
    val testcases = List(
      ("Hello how are you Contestant", 4, "Hello how are you"),
      ("What is the solution to this problem", 4, "What is the solution"),
      ("chopper is not a tanuki", 5, "chopper is not a tanuki")
    )
    for (string, k, expected) <- testcases do assertEquals(StringProblems.truncateSentence(string, k), expected)

  test("1773. Count items matching a rule"):
    val testcases = List(
      (
        (
          List(List("phone", "blue", "pixel"), List("computer", "silver", "lenovo"), List("phone", "gold", "iphone")),
          "color",
          "silver"
        ),
        1
      ),
      (
        (
          List(List("phone", "blue", "pixel"), List("computer", "silver", "phone"), List("phone", "gold", "iphone")),
          "type",
          "phone"
        ),
        2
      )
    )
    for ((items, ruleKey, ruleValue), expected) <- testcases do
      assertEquals(StringProblems.countMatches(items, ruleKey, ruleValue), expected)

  test("1832. Check if sentence is pangram"):
    val testcases = List(
      ("thequickbrownfoxjumpsoverthelazydog", true),
      ("leetcode", false)
    )
    for (input, expected) <- testcases do assertEquals(StringProblems.checkIfPangram(input), expected)

  test("557. Reverse words III"):
    val testcases = List(
      ("Let's take LeetCode contest", "s'teL ekat edoCteeL tsetnoc"),
      ("Mr Ding", "rM gniD")
    )
    for (input, expected) <- testcases do assertEquals(StringProblems.reverseWords(input), expected)

  test("1859. Sorting the sequence"):
    val testcases = List(
      ("Myself2 Me1 I4 and3", "Me Myself and I"),
      ("is2 sentence4 This1 a3", "This is a sentence")
    )
    for (input, expected) <- testcases do assertEquals(StringProblems.sortSentence(input), expected)

  test("2810. Faulty keyboard"):
    val testcases = List(
      ("string", "rtsng"),
      ("poiinter", "ponter")
    )
    for (input, expected) <- testcases do assertEquals(StringProblems.finalString(input), expected)

  test("541. Reverse String II"):
    for (string, k, expected) <-
        List(
          ("abcdefg", 2, "bacdfeg"),
          ("abcd", 2, "bacd")
        )
    do assertEquals(StringProblems.reverseStr(string, k), expected)

  test("2828. Check if a string is an acronym of words"):
    for (words, s, expected) <- List(
        (List("alice", "bob", "charlie"), "abc", true),
        (List("an", "apple"), "a", false),
        (List("never", "gonna", "give", "up", "on", "you"), "ngguoy", true)
      )
    do assertEquals(StringProblems.isAcronym(words, s), expected)

  test("1684. Count the number of consistent strings"):
    for (words, allowed, expected) <- List(
        (Array("ad", "bd", "aaab", "baa", "badab"), "ab", 2),
        (Array("a", "b", "c", "ab", "ac", "bc", "abc"), "abc", 7),
        (Array("cc", "acd", "b", "ba", "bac", "bad", "ac", "d"), "cad", 4)
      )
    do
      assertEquals(
        StringProblems.countConsistentStrings(allowed, words),
        expected,
        clue = s"Expecting '[${words.mkString(",")}]' to contain '${allowed}' '$expected' times."
      )

  test("2418. Sort the people"):
    val testcases = List(
      (Array("Mary", "John", "Emma"), Array(180, 165, 170), Array("Mary", "Emma", "John")),
      (Array("Alice", "Bob", "Bob"), Array(155, 185, 150), Array("Bob", "Alice", "Bob"))
    )
    for (names, heights, expected) <- testcases do
      assertEquals(StringProblems.sortPeople(names, heights).toList, expected.toList)

  test("917. Reverse only letters"):
    for (input, expected) <- List(
        ("ab-cd", "dc-ba"),
        ("a-bC-dEf-ghIj", "j-Ih-gfE-dCba"),
        ("Test1ng-Leet=code-Q!", "Qedo1ct-eeLg=ntse-T!")
      )
    do assertEquals(StringProblems.reverseOnlyLetters(input), expected)

  test("344. Reverse string"):
    val data = "Hello".toCharArray()
    StringProblems.reverseString(data)
    assertEquals(data.toList, "olleH".toCharArray().toList)

  test("1436. Destination city"):
    val testcases = List(
      (List(List("London", "New York"), List("New York", "Lima"), List("Lima", "Sao Paulo")), "Sao Paulo"),
      (List(List("B", "C"), List("D", "B"), List("C", "A")), "A"),
      (List(List("A", "Z")), "Z")
    )
    for (input, expected) <- testcases do assertEquals(StringProblems.destCity(input), expected)

  test("383. Can construct"):
    val testcases = List(
      ("a", "b", false),
      ("aa", "ab", false),
      ("aa", "aab", true)
    )

    for (ransomeNote, magazine, expected) <- testcases do
      assertEquals(
        StringProblems.canConstruct(ransomeNote, magazine),
        expected,
        clue = s"Can construct $ransomeNote from $magazine should be $expected"
      )

  test("2047. Number of valid words in a sentence"):
    val testcases = List(
      ("cat and  dog", 3),
      ("!this  1-s b8d!", 0),
      ("alice and  bob are playing stone-game10", 5)
    )
    for (testcase, expected) <- testcases do
      assertEquals(
        StringProblems.countValidWords(testcase),
        expected,
        clue = s"Expecting $testcase to return $expected"
      )

end StringProblemsSuite
