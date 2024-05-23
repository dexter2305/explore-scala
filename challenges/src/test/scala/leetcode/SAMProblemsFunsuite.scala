package leetcode

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import leetcode.SAMProblems.*
import org.scalacheck.Gen
import org.scalactic.Tolerance.*

class SAMProblemsFunsuite extends AnyFunSuite with ScalaCheckPropertyChecks:

  test("2114. Maximum number of words in a sentence"):
    val testcases = List(
      (Array("please wait", "continue to fight", "continue to win"), 3),
      (Array("alice and bob love leetcode", "i think so too", "this is great thanks very much"), 6),
      (Array("hello world", "unspoken truth of humanity"), 4)
    )
    for testcase <- testcases do
      testcase match
        case (input, expected) => assert(maxNumberOfWords(input) === expected)

  test("1108. Defanging IP address"):
    val testCases = List(
      ("1.1.1.1", "1[.]1[.]1[.]1"),
      ("255.100.50.0", "255[.]100[.]50[.]0")
    )
    for (input, expected) <- testCases do assert(defangingIPAddress(input) === expected)

  test("709. To Lowercase"):
    val testcases = List(
      ("Must BE LOweR", "must be lower")
    )
    for (input, e) <- testcases do assert(toLowerCase(input) === e)

  test("2129. Capitalize the title"):
    val testcases = List(
      ("This is a Valid Title", "This is a Valid Title"),
      ("capiTalIze tHe titLe", "Capitalize The Title"),
      ("First leTTeR of EACH Word", "First Letter of Each Word"),
      ("i lOve leetcode", "i Love Leetcode")
    )
    for (input, expected) <- testcases do assert(capitalizeTheTitle(input) === expected)

  test("520. Detect capital"):
    val testcases = List(
      ("INDIA", true),
      ("india", true),
      ("India", true),
      ("InDIa", false)
    )
    for (input, expected) <- testcases do assert(detectCapital(input) === expected)

  test("771. Jewels and stones"):
    val testcases = List(
      (("aA", "aAAbbbb"), 3),
      (("z", "ZZ"), 0)
    )
    for ((jewels, stones), expected) <- testcases do
      assert(
        jewelsAndStones(jewels, stones) === expected,
        clue = s"Jewels: '$jewels' && stones:'$stones' must return '$expected'"
      )

  test("2011. Final value after performing operations"):
    val testcases = List(
      (Array("--X", "X++", "X++"), 1),
      (Array("++X", "++X", "X++"), 3),
      (Array("X++", "++X", "--X", "X--"), 0)
    )
    for (input, expected) <- testcases do
      assert(
        finalValueAfterOps(input) === expected,
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
        findWordsContaining(words, x).sameElements(expected),
        clue =
          s"Expecting '${words.mkString(",")}' to contain '$x' in indices '${expected.mkString(",")}'"
      )

  test("1662. Check if two strings are equivalent"):
    val testcases = List(
      ((Array("ab", "c"), Array("a", "bc")), true),
      ((Array("a", "cb"), Array("ab", "c")), false),
      ((Array("abc", "d", "defg"), Array("abcddefg")), true)
    )
    for ((word1, word2), expected) <- testcases do
      assert(
        arrayStringsAreEqual(word1, word2) === expected,
        clue = s"Expecting equivalence between word1:'${word1.mkString(",")}' & word2:'${word2
            .mkString(",")}' as '$expected'"
      )

  test("1816. Truncate sentence"):
    val testcases = List(
      ("Hello how are you Contestant", 4, "Hello how are you"),
      ("What is the solution to this problem", 4, "What is the solution"),
      ("chopper is not a tanuki", 5, "chopper is not a tanuki")
    )
    for (string, k, expected) <- testcases do assert(truncateSentence(string, k) === expected)

  test("1773. Count items matching a rule"):
    val testcases = Table(
      ("product list", "rule key", "rule value", "expected"),
      (
        List(
          List("phone", "blue", "pixel"),
          List("computer", "silver", "lenovo"),
          List("phone", "gold", "iphone")
        ),
        "color",
        "silver",
        1
      ),
      (
        List(
          List("phone", "blue", "pixel"),
          List("computer", "silver", "phone"),
          List("phone", "gold", "iphone")
        ),
        "type",
        "phone",
        2
      )
    )
    forAll(testcases): (items, ruleKey, ruleValue, expected) =>
      assert(countMatches(items, ruleKey, ruleValue) === expected)

  test("1832. Check if sentence is pangram"):
    val testcases = List(
      ("thequickbrownfoxjumpsoverthelazydog", true),
      ("leetcode", false)
    )
    for (input, expected) <- testcases do assert(checkIfPangram(input) === expected)

  test("557. Reverse words III"):
    val testcases = List(
      ("Let's take LeetCode contest", "s'teL ekat edoCteeL tsetnoc"),
      ("Mr Ding", "rM gniD")
    )
    for (input, expected) <- testcases do assert(reverseWordsIII(input) === expected)

  test("1859. Sorting the sequence"):
    val testcases = List(
      ("Myself2 Me1 I4 and3", "Me Myself and I"),
      ("is2 sentence4 This1 a3", "This is a sentence")
    )
    for (input, expected) <- testcases do assert(sortSentence(input) === expected)

  test("2810. Faulty keyboard"):
    val testcases = List(
      ("string", "rtsng"),
      ("poiinter", "ponter")
    )
    for (input, expected) <- testcases do assert(finalString(input) === expected)

  test("541. Reverse String II"):
    for (string, k, expected) <-
        List(
          ("abcdefg", 2, "bacdfeg"),
          ("abcd", 2, "bacd")
        )
    do assert(reverseStr(string, k) === expected)

  test("2828. Check if a string is an acronym of words"):
    for (words, s, expected) <- List(
        (List("alice", "bob", "charlie"), "abc", true),
        (List("an", "apple"), "a", false),
        (List("never", "gonna", "give", "up", "on", "you"), "ngguoy", true)
      )
    do assert(isAcronym(words, s) === expected)

  test("1684. Count the number of consistent strings"):
    for (words, allowed, expected) <- List(
        (Array("ad", "bd", "aaab", "baa", "badab"), "ab", 2),
        (Array("a", "b", "c", "ab", "ac", "bc", "abc"), "abc", 7),
        (Array("cc", "acd", "b", "ba", "bac", "bad", "ac", "d"), "cad", 4)
      )
    do
      assert(
        countConsistentStrings(allowed, words) === expected,
        clue = s"Expecting '[${words.mkString(",")}]' to contain '${allowed}' '$expected' times."
      )

  test("2418. Sort the people"):
    val testcases = List(
      (Array("Mary", "John", "Emma"), Array(180, 165, 170), Array("Mary", "Emma", "John")),
      (Array("Alice", "Bob", "Bob"), Array(155, 185, 150), Array("Bob", "Alice", "Bob"))
    )
    for (names, heights, expected) <- testcases do
      assert(sortPeople(names, heights).toList === expected.toList)

  test("917. Reverse only letters"):
    for (input, expected) <- List(
        ("ab-cd", "dc-ba"),
        ("a-bC-dEf-ghIj", "j-Ih-gfE-dCba"),
        ("Test1ng-Leet=code-Q!", "Qedo1ct-eeLg=ntse-T!")
      )
    do assert(reverseOnlyLetters(input) === expected)

  test("344. Reverse string"):
    val data = "Hello".toCharArray()
    reverseString(data)
    assert(data.toList === "olleH".toCharArray().toList)

  test("1436. Destination city"):
    val testcases = List(
      (
        List(List("London", "New York"), List("New York", "Lima"), List("Lima", "Sao Paulo")),
        "Sao Paulo"
      ),
      (List(List("B", "C"), List("D", "B"), List("C", "A")), "A"),
      (List(List("A", "Z")), "Z")
    )
    for (input, expected) <- testcases do assert(destCity(input) === expected)

  test("383. Can construct"):
    val testcases = List(
      ("a", "b", false),
      ("aa", "ab", false),
      ("aa", "aab", true)
    )

    for (ransomeNote, magazine, expected) <- testcases do
      assert(
        canConstruct(ransomeNote, magazine) === expected,
        clue = s"Can construct $ransomeNote from $magazine should be $expected"
      )

  test("2047. Number of valid words in a sentence"):
    val testcases = Table(
      ("sentence", "valid token count"),
      ("123", 0),
      ("a", 1),
      ("cat and  dog", 3),
      ("!this  1-s b8d!", 0),
      ("alice and  bob are playing stone-game10", 5),
      ("0", 0),
      ("he bought 2 pencils, 3 erasers, and 1  pencil-sharpener.", 6),
      ("-", 0),
      (" o6 r", 1),
      (". ! 7hk  al6 l! aon49esj35la k3 7u2tkh  7i9y5  !jyylhppd et v- h!ogsouv 5", 4),
      ("b-a-c f-d", 1)
    )
    forAll(testcases): (testcase, expected) =>
      assert(
        countValidWords(testcase) === expected,
        clue = s"Expected '${testcase}' to return '$expected'"
      )

  test("2138. Divide a string into groups of size k"):
    val testcases = Table(
      ("s", "k", "fill", "expected"),
      ("abcdefghi", 3, 'x', Array("abc", "def", "ghi")),
      ("abcdefghij", 3, 'x', Array("abc", "def", "ghi", "jxx"))
    )
    forAll(testcases): (s, k, fill, expected) =>
      assert(divideString(s, k, fill).toList === expected.toList)

  test("2108. Find first palindromic string in the array"):
    val testcases = Table(
      ("given-array", "expected"),
      (Array("abc", "car", "ada", "racecar", "cool"), "ada"),
      (Array("notapalindrome", "racecar"), "racecar"),
      (Array("def", "ghi"), "")
    )
    forAll(testcases): (givenArray, expected) =>
      assert(
        firstPalindrome(givenArray) === expected,
        clue = s"Expected '${givenArray.mkString(",")}' to return '$expected'."
      )

  test("125. Valid Palindrome"):
    val testcases = Table(
      ("string", "expectation"),
      ("A man, a plan, a canal: Panama", true),
      ("race a car", false),
      (" ", true),
      ("palap", true),
      ("paap", true),
      ("0P", false)
    )
    forAll(testcases): (string, expected) =>
      assert(
        isPalindrome(string) === expected,
        clue = "Expected '$string' to return '$expected'"
      )

  test("434. Number of segments in a string"):
    val testcases = Table(
      ("string", "expected"),
      ("Hello, my name is John", 5),
      ("Hello", 1),
      ("", 0),
      (", , , ,        a, eaefa", 6)
    )
    forAll(testcases): (string, expected) =>
      assert(
        countSegments(string) === expected,
        clue = s"For string '$string', expected value is '$expected'."
      )

  test("1768. Merge string alternatively"):
    val testcases = Table(
      ("word1", "word", "expected"),
      ("abc", "pqr", "apbqcr"),
      ("ab", "pqrs", "apbqrs"),
      ("abcd", "pq", "apbqcd"),
      ("a", "b", "ab"),
      (" ", "b", " b"),
      ("a", " ", "a ")
    )
    forAll(testcases): (word1, word2, expected) =>
      assert(mergeAlternately(word1, word2) === expected)

  test("1614. Maximum nesting depth of parantheses"):
    val testcases = Table(
      ("string", "expected"),
      ("(1+(2*3)+((8)/4))+1", 3),
      ("(1)+((2))+(((3)))", 3),
      ("", 0),
      ("()(()())", 2),
      ("()()", 1)
    )
    forAll(testcases): (string, expectation) =>
      assert(
        maxDepth(string) === expectation,
        clue = s"Depth of '$string' is '$expectation'"
      )

  test("2710. Remove trailing zeroes from a string"):
    val testcases = Table(
      ("num", "expected"),
      ("51230100", "512301"),
      ("123", "123"),
      ("0", ""),
      ("00000", "")
    )
    forAll(testcases): (num, expected) =>
      assert(removeTrailingZeros(num) === expected)

  test("2678. Number of senior citizens"):
    val testcases = Table(
      ("details", "expected"),
      (Array("7868190130M7522", "5303914400F9211", "9273338290F4010"), 2),
      (Array("1313579440F2036", "2921522980M5644"), 0)
    )
    forAll(testcases): (details, expected) =>
      assert(countSeniors(details) === expected)

  test("1544. Make the string great"):
    val testcases = Table(
      ("given string", "expected"),
      ("leetcode", "leetcode"),
      ("s", "s"),
      ("", ""),
      ("abBAcC", ""),
      ("leEeetcode", "leetcode")
    )
    forAll(testcases): (s, expected) =>
      assert(makeGood(s) === expected)

  test("58. Length of last word"):
    val testcases = Table(
      ("sentence", "expected"),
      ("Hello World", 5),
      ("   fly me   to   the moon  ", 4),
      ("luffy is still joyboy", 6),
      ("a", 1),
      ("", 0),
      ("hello world   ", 5)
    )
    forAll(testcases): (sentence, expected) =>
      assert(lengthOfLastWord(sentence) === expected)

  test("28. Find index of the first occurence in a string"):
    val testcases = Table(
      ("haystack", "needle", "expected"),
      ("sadbutsad", "sad", 0),
      ("leetcode", "leeto", -1)
    )
    forAll(testcases): (haystack, needle, expected) =>
      assert(strStr(haystack, needle) === expected)

  test("20. Valid parentheses"):
    val testcases = Table(
      ("parentheses string", "expected"),
      ("()", true),
      ("()[]{}", true),
      ("(]", false),
      ("]", false),
      ("}", false),
      (")", false)
    )
    forAll(testcases): (input, expected) =>
      assert(isValid(input) === expected)

  test("1249. Minimum remove to make valid parantheses"):
    val testcases = Table(
      ("input", "expected"),
      ("lee(t(c)o)de)", "lee(t(c)o)de"),
      ("a)b(c)d", "ab(c)d"),
      ("))((", ""),
      ("((", ""),
      ("(()", "()"),
      ("allgood", "allgood")
    )
    forAll(testcases): (string, expected) =>
      assert(minRemoveToMakeValid(string) === expected)

  test("1678. Goal parser interpretation"):
    val testcases = Table(
      ("command", "expected"),
      ("G()(al)", "Goal"),
      ("G()()()()(al)", "Gooooal"),
      ("(al)G(al)()()G", "alGalooG")
    )
    forAll(testcases): (command, expected) =>
      assert(interpret(command) === expected)

  test("678. Valid paranthesis string"):
    pendingUntilFixed:
      val testcases = Table(
        ("string", "isValid"),
        ("()", true),
        (")(", false),
        ("(*))", true),
        ("(*)", true),
        ("*", true),
        (")()", false),
        (")", false),
        ("(", false),
        ("()(", false),
        (
          "(((((*(()((((*((**(((()()*)()()()*((((**)())*)*)))))))(())(()))())((*()()(((()((()*(())*(()**)()(())",
          false
        )
      )
      forAll(testcases): (string, expected) =>
        assert(checkValidString(string) === expected)

  test("389. Find the difference"):
    val testcases = Table(
      ("s", "t", "appended char"),
      ("abcd", "abcde", 'e'),
      ("", "y", 'y')
    )
    forAll(testcases): (s, t, expected) =>
      assert(findTheDifference(s, t) == expected)

  test("2000. Reverse prefix of word"):
    val testcases = Table(
      ("word", "ch", "expected"),
      ("abcdefd", 'd', "dcbaefd"),
      ("xyxzxe", 'z', "zxyxxe"),
      ("abcd", 'z', "abcd"),
      ("abcd", 'd', "dcba")
    )
    forAll(testcases): (word, ch, expected) =>
      assert(reversePrefix(word, ch) === expected)

  test("3019. Number of changing keys"):
    val testcases = Table(
      ("string", "expected"),
      ("aAbBcC", 2),
      ("AaAaAaaA", 0),
      ("a", 0)
    )
    forAll(testcases): (string, expected) =>
      assert(countKeyChanges(string) === expected)

  test("1221. Split a string in balanced string"):
    val testcases = Table(
      ("s", "expected"),
      ("RLRRLLRLRL", 4),
      ("RLRRRLLRLL", 2),
      ("LLLLRRRR", 1),
      ("RR", 0),
      ("LL", 0),
      ("RL", 1),
      ("LR", 1)
    )
    forAll(testcases): (s, expected) =>
      assert(balancedStringSplit(s) === expected)

  test("1528. Shufftle string"):
    val testcases = Table(
      ("s", "indices", "expected"),
      ("codeleet", Array(4, 5, 6, 7, 0, 2, 1, 3), "leetcode"),
      ("abc", Array(0, 1, 2), "abc")
    )
    forAll(testcases): (string, array, expected) =>
      assert(restoreString(string, array) === expected)

  test("2325. Decode the message"):
    val testcases = Table(
      ("key", "message", "expected"),
      ("the quick brown fox jumps over the lazy dog", "vkbs bs t suepuv", "this is a secret"),
      (
        "eljuxhpwnyrdgtqkviszcfmabo",
        "zwx hnfx lqantp mnoeius ycgk vcnjrdb",
        "the five boxing wizards jump quickly"
      )
    )
    forAll(testcases): (key, message, expectedDecodedMessage) =>
      assert(decodeMessage(key, message) === expectedDecodedMessage)

  test("67. Add binary"):
    // example based test
    val testcases = Table(
      ("x", "y", "expected sum"),
      ("0", "0", "0"),
      ("1", "1", "10"),
      ("11", "1", "100"),
      ("1010", "1011", "10101")
    )
    forAll(testcases): (x, y, expected) =>
      assert(addBinary(x, y) === expected)

    // property based test to cover wider range
    given PropertyCheckConfiguration(minSuccessful = 50000, maxDiscardedFactor = 100)
    val intGenerator = for n <- Gen.choose(0, Int.MaxValue) yield n
    forAll((intGenerator, "a"), (intGenerator, "b")): (a: Int, b: Int) =>
      whenever(a >= 0 && a < Int.MaxValue && b >= 0 && b != Int.MinValue):
        val expectedString = (a + b).toBinaryString
        assert(
          addBinary(a.toBinaryString, b.toBinaryString) === (a + b).toBinaryString
        )

  test("415. Add strings"):
    // example based tests
    val testcases = Table(
      ("a", "b", "expected sum"),
      ("11", "123", "134"),
      ("456", "77", "533"),
      ("0", "0", "0")
    )
    forAll(testcases): (a, b, expected) =>
      assert(addStrings(a, b) === expected)

    given PropertyCheckConfiguration(minSuccessful = 1000, maxDiscardedFactor = 1)
    // property based tests
    val intGenerator = for n <- Gen.choose(0, 9999) yield n

    forAll((intGenerator, "a"), (intGenerator, "b")): (a: Int, b: Int) =>
      whenever(a > 0 && a < Int.MaxValue && b > 0 && b < Int.MaxValue):
        assert(addStrings(a.toString(), b.toString) === (a + b).toString)

  test("2864. Maximum odd binary number"):
    val testcases = Table(
      ("010", "001"),
      ("0101", "1001"),
      ("1", "1")
    )
    forAll(testcases): (string, expected) =>
      assert(maximumOddBinaryNumber(string) === expected)

  test("804. Unique morse code of words."):
    val testcases = Table(
      ("words", "expeced unique transformations"),
      (List("gin", "zen", "gig", "msg"), 2),
      (List("a"), 1)
    )
    forAll(testcases): (words, expected) =>
      assert(uniqueMorseRepresentations(words.toArray) === expected)

  test("242. Valid anagram."):
    val testcases = Table(
      ("s", "t", "anagram"),
      ("anagram", "nagaram", true),
      ("rat", "car", false)
    )
    forAll(testcases): (s, t, expected) =>
      assert(isAnagram(s, t) === expected)

  test("205. Isomorphic strings"):
    val testcases = Table(
      ("s", "t", "isomorphic?"),
      // ("egg", "add", true),
      // ("foo", "bar", false),
      // ("paper", "title", true),
      // ("badc", "baba", false),
      ("aaeaa", "uuxyy", false)
    )
    forAll(testcases): (s, t, expected) =>
      assert(isIsomorphic(s, t) === expected)

  test("412. Fizz Buzz"):
    val testcases = Table(
      ("n", "expected"),
      (1, List("1")),
      (2, List("1", "2")),
      (3, List("1", "2", "Fizz")),
      (5, List("1", "2", "Fizz", "4", "Buzz"))
    )
    forAll(testcases): (input, expected) =>
      assert(fizzBuzz(input) === expected)

  test("387. First unique character in a string."):
    val testcases = Table(
      ("string", "expected index"),
      ("leetcode", 0),
      ("loveleetcode", 2),
      ("aabb", -1)
    )
    forAll(testcases): (string, expectedIndex) =>
      assert(firstUniqChar(string) === expectedIndex)

  test("1556. Thousand separator."):
    val testcases = Table(
      ("n", "expected"),
      (987, "987"),
      (1000, "1.000")
    )
    forAll(testcases): (n, expected) =>
      assert(thousandSeparator(n) === expected)

  test("1805. Number of different integers in a string."):
    val testcases = Table(
      ("string", "expected number of ints"),
      ("a123bc34d8ef34", 3),
      ("leet1234code234", 2),
      ("a1b01c001", 1),
      ("u", 0),
      ("gi851a851q8510v", 2),
      ("ab101ab10cd10", 2),
      ("ab1000cd1000", 1),
      ("ab101cd11", 2),
      ("167278959591294", 1),
      ("000", 1),
      ("0b0", 1),
      ("010a", 1)
    )
    forAll(testcases): (string, expected) =>
      assert(numDifferentIntegers(string) === expected)

  test("1455. Check if a word occurs as a prefix of a word in a sentence."):
    val testcases = Table(
      ("sentence", "searchWord", "expectedIndex"),
      ("i love eating burger", "burg", 4),
      ("this problem is an easy problem", "pro", 2),
      ("i am tired", "you", -1),
      ("trick", "t", 1)
    )
    forAll(testcases): (sentence, searchWord, expectedIndex) =>
      assert(isPrefixOfWord(sentence = sentence, searchWord) === expectedIndex)

  test("2185. Counting words with a given prefix"):
    val testcases = Table(
      ("words", "prefix", "expected"),
      (List("pay", "attention", "practice", "attend"), "at", 2),
      (List("leetcode", "win", "loops", "success"), "code", 0)
    )
    forAll(testcases): (words, prefix, expected) =>
      assert(prefixCount(words.toArray, prefix) === expected)

  test("2124. Check if all A's appear before all B's"):
    val testcases = Table(
      ("string", "expected"),
      ("aaabbb", true),
      ("abab", false),
      ("bbb", true),
      ("aa", true),
      ("a", true),
      ("b", true)
    )
    forAll(testcases): (string, expected) =>
      assert(checkString(string) == expected)

  test("500. Keyboard row"):
    val testcases = Table(
      ("words", "expected"),
      (List("Hello", "Alaska", "Dad", "Peace"), List("Alaska", "Dad")),
      (List("omk"), List.empty[String]),
      (List("adsdf", "sfd"), List("adsdf", "sfd"))
    )
    forAll(testcases): (words, expected) =>
      assert(findWords(words.toArray).toList === expected)

  test("2423. Remove letter to equalize frequency"):
    val testcases = Table(
      ("string", "expected"),
      ("abcc", true),
      ("aabb", false),
      ("abbcc", true),
      ("abc", true),
      ("aa", true),
      ("abbbcc", false)
    )
    forAll(testcases): (string, expected) =>
      assertResult(expected):
        equalFrequency(string)

  test("3110. Score of a string"):
    val testcases = Table(
      ("string", "expected score"),
      ("hello", 13),
      ("zaz", 50)
    )
    forAll(testcases): (string, expectedScore) =>
      assert(scoreOfString(string) === expectedScore)

  test("2194. Cells in a range on an excel sheet."):
    val testcases = Table(
      ("s", "expected range"),
      ("K1:L2", List("K1", "K2", "L1", "L2")),
      ("A1:F1", List("A1", "B1", "C1", "D1", "E1", "F1"))
    )
    forAll(testcases): (s, expected) =>
      assert(cellsInRange(s) === expected)

  test("824. Goat latin."):
    val testcases = Table(
      ("sentence", "expected"),
      ("I speak Goat Latin", "Imaa peaksmaaa oatGmaaaa atinLmaaaaa"),
      (
        "The quick brown fox jumped over the lazy dog",
        "heTmaa uickqmaaa rownbmaaaa oxfmaaaaa umpedjmaaaaaa overmaaaaaaa hetmaaaaaaaa azylmaaaaaaaaa ogdmaaaaaaaaaa"
      ),
      (
        "Each word consists of lowercase and uppercase letters only",
        "Eachmaa ordwmaaa onsistscmaaaa ofmaaaaa owercaselmaaaaaa andmaaaaaaa uppercasemaaaaaaaa etterslmaaaaaaaaa onlymaaaaaaaaaa"
      )
    )
    forAll(testcases): (s, expected) =>
      assert(toGoatLatin(s) === expected)

  test("1002. Find common characters."):
    val testcases = Table(
      ("words", "expected common chars"),
      (List("bella", "label", "roller"), List("e", "l", "l")),
      (List("cool", "lock", "cook"), List("c", "o")),
      (List("a", "aaa"), List("a"))
    )
    forAll(testcases): (words, expected) =>
      assert(commonChars(words.toArray).sorted === expected.sorted)

  test("2716. Minimize string length."):
    val testcases = Table(
      ("string", "expected length"),
      ("aaabc", 3),
      ("cbbd", 3),
      ("dddaaa", 2)
    )
    forAll(testcases): (string, expectedLength) =>
      assert(minimizedStringLength(string) === expectedLength)

  test("1309. Decrypt string from alphabet to integer mapping."):
    val testcases = Table(
      ("string", "expected"),
      ("10#11#12", "jkab"),
      ("1326#", "acz"),
      ("1", "a"),
      ("26", "bf"),
      ("26#", "z"),
      ("10#126#", "jaz")
    )
    forAll(testcases): (string, expected) =>
      assert(freqAlphabets(string) === expected)

  test("1844. Replace all digits with characters"):
    val testcases = Table(
      ("s", "expected"),
      ("a1c1e1", "abcdef"),
      ("a1b2c3d4e", "abbdcfdhe"),
      ("a", "a"),
      ("a3c", "adc")
    )
    forAll(testcases): (string, expected) =>
      assert(replaceDigits(string) === expected)

  test("2744. Find the maximum number of string pairs"):
    val testcases = Table(
      ("words", "expected pairs"),
      (List("cd", "ac", "dc", "ca", "zz"), 2),
      (List("ab", "ba", "cc"), 1),
      (List("aa", "ab"), 0)
    )
    forAll(testcases): (words, expected) =>
      assert(maximumNumberOfStringPairs(words.toArray) === expected)

  test("13. Roman to integer."):
    val basicExamples = Table(
      ("string", "expected"),
      ("I", 1),
      ("V", 5),
      ("X", 10),
      ("L", 50),
      ("C", 100),
      ("D", 500),
      ("M", 1000)
    )
    val incrementExamples = Table(
      ("string", "expected"),
      ("III", 3),
      ("VIII", 8),
      ("XII", 12),
      ("LII", 52),
      ("CIII", 103),
      ("DIII", 503),
      ("MI", 1001)
    )
    val decrementExamples = Table(
      ("string", "expected"),
      ("IV", 4),
      ("IX", 9),
      ("IL", 49),
      ("IC", 99),
      ("ID", 499),
      ("IM", 999)
    )
    val complexExamples = Table(
      ("string", "expected"),
      ("MCMXCIV", 1994)
    )
    forAll(basicExamples ++ incrementExamples ++ decrementExamples ++ complexExamples):
      (roman, expected) => assert(romanToInt(roman) === expected)

  test("1417. Reformat the string."):
    val testcases = Table(
      ("string", "expected in"),
      ("a0b1c2", true),
      ("leetcode", false),
      ("1229857369", false),
      ("ab12", true),
      ("1a23", false),
      ("abc12", true),
      ("1d2ef", true),
      ("covid2019", true)
    )
    forAll(testcases): (string, expectation) =>
      if !expectation then assert(reformat(string) === "")
      else
        val actual = reformat(string)
        val eval = actual
          .sliding(2)
          .map(s => (s(0), s(1)))
          .filter(t => t._1.isLetterOrDigit && t._2.isLetterOrDigit)
          .map: (x, y) =>
            val validity = (x.isDigit && y.isLetter) || (x.isLetter && y.isDigit)
            (validity, s"$x$y")
          .toList
        println(s"$eval")
        assert(
          eval.forall(_._1) === true,
          clue = s"'$actual' is invalid because of '${eval.filter(_._1 == false).map(_._2)}' "
        )

  test("345. Reverse vowels of a string."):
    val testcases = Table(
      ("string", "expected"),
      ("hello", "holle"),
      ("leetcode", "leotcede"),
      ("abc", "abc"),
      ("aeiou", "uoiea"),
      ("aA", "Aa")
    )
    forAll(testcases): (givenString, expected) =>
      assert(reverseVowels(givenString) === expected)

  test("1941. Check if all characters have equal occurences."):
    val testcases = Table(
      ("string", "expected"),
      ("abacbc", true),
      ("aaabb", false),
      ("a", true)
    )
    forAll(testcases): (s, expected) =>
      assert(areOccurrencesEqual(s) === expected)

  test("2315. Count asterisks"):
    val testcases = Table(
      ("s", "expected"),
      ("l|*e*et|c**o|*de|", 2),
      ("iamprogrammer", 0),
      ("yo|uar|e**|b|e***au|tifu|l", 5),
      ("*", 1),
      ("*|ig*nore|*|e*f|*", 3)
    )
    forAll(testcases): (s, expected) =>
      assert(countAsterisks(s) === expected)

  test("2255. Count prefixes of a given string"):
    val testcases = Table(
      ("words", "s", "expected"),
      (List("a", "b", "c", "ab", "bc", "abc"), "abc", 3),
      (List("a", "a"), "aa", 2)
    )
    forAll(testcases): (words, s, expected) =>
      assert(countPrefixes(words.toArray, s) === expected)

  test("2278. Percentage of letter in string"):
    val testcases = Table(
      ("s", "letter", "expected"),
      ("foobar", 'o', 33),
      ("jjjj", 'k', 0)
    )
    forAll(testcases): (s, letter, expected) =>
      assert(percentageLetter(s, letter) === expected)

  test("1071. Greatest common divisor of strings."):
    val testcases = Table(
      ("string1", "string2", "expected"),
      ("ABCABCABC", "ABC", "ABC"),
      ("ABABAB", "ABAB", "AB"),
      ("LEET", "CODE", "")
    )
    forAll(testcases): (a, b, expected) =>
      assert(gcdOfStrings(a, b) === expected)

  test("151. Reverse words in a string."):
    val testcases = Table(
      ("string", "expected"),
      ("good", "good"),
      ("   hello  ", "hello"),
      ("  hello world  ", "world hello"),
      ("the sky is blue", "blue is sky the"),
      ("a good   example", "example good a")
    )
    forAll(testcases): (string, expect) =>
      assert(reverseWords(string) === expect)

  test("443. String compression."):
    val testcases = Table(
      ("string", "expected"),
      (List("a"), 1),
      (List("a", "a"), 2),
      (List("a", "a", "b", "b", "c", "c", "c"), 6),
      (List("a", "b", "b", "b", "b", "b", "b", "b", "b", "b", "b", "b", "b"), 4)
    )
    forAll(testcases): (string, expectedLength) =>
      assert(compress(string.map(_.head).toArray) === expectedLength)

  test("2351. First letter to appear twice."):
    val testcases = Table(
      ("string", "char"),
      ("abccbaacz", 'c'),
      ("abcdd", 'd'),
      ("aa", 'a'),
      ("aabaa", 'a')
    )
    forAll(testcases): (string, expected) =>
      assert(repeatedCharacter(string) === expected)

  test("2788. Split string by separator."):
    val testcases = Table(
      ("words", "separator", "expected"),
      (
        List("one.two.three", "four.five", "six"),
        '.',
        List("one", "two", "three", "four", "five", "six")
      ),
      (List("$easy$", "$problem$"), '$', List("easy", "problem")),
      (List("|||"), '|', List.empty[String])
    )
    forAll(testcases): (words, sep, exp) =>
      assert(splitWordsBySeparator(words, sep) === exp)

  test("459. Repeated substring pattern."):
    val testcases = Table(
      ("string", "expected"),
      ("abab", true),
      ("aba", false),
      ("ababa", false),
      ("abcabcabcabc", true),
      ("ababab", true),
      ("aabbccddee", false),
      ("bb", true)
    )
    forAll(testcases): (string, expected) =>
      assertResult(expected):
        repeatedSubstringPattern(string)

  test("657. Robot return to origin."):
    val testcases = Table(
      ("moves", "expected"),
      ("UD", true),
      ("LL", false)
    )
    forAll(testcases): (moves, expected) =>
      assertResult(expected):
        judgeCircle(moves)

  test("1041. Robot bounded in a circle."):
    val testcases = Table(
      ("instructions", "expected"),
      ("G", false),
      ("GL", true),
      ("GGLLGG", true),
      ("GLRLLGLL", true)
    )
    forAll(testcases): (ins, expected) =>
      assertResult(expected):
        isRobotBounded(ins)

  test("2073. Time needed to buy tickets"):
    val testcases = Table(
      ("tickets", "k", "expected"),
      (Array(2, 3, 2), 2, 6),
      (Array(5, 1, 1, 1), 0, 8)
    )
    forAll(testcases): (tickets, k, expected) =>
      assert(timeRequiredToBuy(tickets, k) === expected)

  test("463. Island perimter"):
    val testcases = Table(
      ("grid", "expected perimeter"),
      (Array(Array(0, 1, 0, 0), Array(1, 1, 1, 0), Array(0, 1, 0, 0), Array(1, 1, 0, 0)), 16),
      (Array(Array(1)), 4),
      (Array(Array(1, 0)), 4)
    )
    forAll(testcases): (grid, expected) =>
      assert(islandPerimeter(grid) === expected)

  test("1431. Kids with greatest number of candies"):
    val testcases = Table(
      ("candies", "extras", "expected"),
      (List(2, 3, 5, 1, 3), 3, List(true, true, true, false, true)),
      (List(4, 2, 1, 1, 2), 1, List(true, false, false, false, false)),
      (List(12, 1, 12), 1, List(true, false, true))
    )
    forAll(testcases): (candies, extras, expected) =>
      assert(kidsWithCandies(candies.toArray, extras) === expected)

  test("605. Can place flowers."):
    val testcases = Table(
      ("flowerbed", "extras", "expected"),
      (List(1, 0, 0, 0, 1), 1, true),
      (List(1, 0, 0, 0, 1), 2, false),
      (List(1, 0, 0, 0, 1), 0, true),
      (List(0, 0, 0, 0, 0), 2, true),
      (List(1), 1, false),
      (List(1, 0, 1, 0), 1, false),
      (List(1, 0, 0, 0), 1, true),
      (List(0, 1, 0), 1, false),
      (List(0), 1, true)
    )
    forAll(testcases): (bed, extra, expected) =>
      assert(canPlaceFlowers(bed.toArray, extra) === expected)

  test("238. Product of array except self."):
    val testcases = Table(
      ("nums", "expected"),
      (List(1, 2, 3, 4), List(24, 12, 8, 6)),
      (List(-1, 1, 0, -3, 3), List(0, 0, 9, 0, 0))
    )
    forAll(testcases): (nums, expected) =>
      assert(productExceptSelf(nums.toArray) === expected)

  test("283. Move zeroes."):
    val testcases = Table(
      ("array", "expected"),
      (List(0, 1, 0, 3, 12), List(1, 3, 12, 0, 0)),
      (List(0), List(0)),
      (List(1, 2, 3), List(1, 2, 3)),
      (List(1), List(1)),
      (List(0, 0, 0, 0, 5), List(5, 0, 0, 0, 0))
    )
    forAll(testcases): (ints, expected) =>
      val array = ints.toArray
      moveZeroes(array)
      assert(array.toList === expected)

  test("66. Plus One."):
    val testcases = Table(
      ("digits", "expected"),
      (List(1, 2, 3), List(1, 2, 4)),
      (List(1), List(2)),
      (List(9), List(1, 0)),
      (List(4, 3, 2, 1), List(4, 3, 2, 2)),
      (List.empty[Int], List(1))
    )
    forAll(testcases): (digits, expected) =>
      assert(plusOne(digits.toArray).toList === expected)

  test("1822. Sign of the product of the array."):
    val testcases = Table(
      ("array", "expected"),
      (List(-1, -2, -3, -4, 3, 2, 1), 1),
      (List(1, 5, 0, 2, -3), 0),
      (List(-1, 1, -1, 1, -1), -1),
      (
        List(41, 65, 14, 80, 20, 10, 55, 58, 24, 56, 28, 86, 96, 10, 3, 84, 4, 41, 13, 32, 42, 43,
          83, 78, 82, 70, 15, -41),
        -1
      ),
      (List(3), 1)
    )
    forAll(testcases): (ints, expected) =>
      assert(arraySign(ints.toArray) === expected)

  test("1502. Can make arithmetic sequence from sequence."):
    val testcases = Table(
      ("array", "result"),
      (List(3, 5, 1), true),
      (List(1, 2, 4), false),
      (List(1, 100), true)
    )
    forAll(testcases): (array, expected) =>
      assert(canMakeArithmeticProgression(array.toArray) === expected)

  test("896. Monotonic array."):
    val testcases = Table(
      ("array", "expected"),
      (List(1, 2, 3), true),
      (List(6, 5, 4), true),
      (List(1, 3, 2), false),
      (List(1, 2, 2, 3), true),
      (List(3, 2, 2, 1), true),
      (List(1, 1, 1, 2), true),
      (List(1, 1), true),
      (List(1), true),
      (List(1, 1, 1), true)
    )
    forAll(testcases): (ints, expected) =>
      assertResult(expected):
        isMonotonic(ints.toArray)

  test("682. Baseball game"):
    val testcases = Table(
      ("operations", "expected sum"),
      (List("5", "2", "C", "D", "+"), 30),
      (List("5", "-2", "4", "C", "D", "9", "+", "+"), 27),
      (List("1", "C"), 0)
    )
    forAll(testcases): (ops, expected) =>
      assertResult(expected):
        calPoints(ops.toArray)

  test("2441. Largest positive integer that exists with its negative"):
    val testcases = Table(
      ("ints", "expected"),
      (List(-1, 2, -3, 3), 3),
      (List(-1, 10, 6, 7, -7, 1), 7),
      (List(-10, 8, 6, 7, -2, -3), -1)
    )
    forAll(testcases): (ints, expected) =>
      assertResult(expected):
        findMaxK(ints.toArray)

  test("1672. Richest customer wealth."):
    val testcases = Table(
      ("wealths", "expected"),
      (List(List(1, 2, 3), List(3, 2, 1)), 6),
      (List(List(1, 5), List(7, 3), List(3, 5)), 10),
      (List(List(2, 8, 7), List(7, 1, 3), List(1, 9, 5)), 17)
    )
    forAll(testcases): (wealths, expected) =>
      assertResult(expected):
        maximumWealth(wealths.map(_.toArray).toArray)

  test("165. Compare  version numbers."):
    val testcases = Table(
      ("version1", "version2", "expected"),
      ("1.01", "1.001", 0),
      ("1.02", "1.001", 1),
      ("1.02", "1.03", -1),
      ("1.02", "1.02", 0),
      ("0.1", "1.1", -1),
      ("1.0.0", "1.0", 0),
      ("1.0", "1.0.0", 0),
      ("0.0.5", "0.1.0", -1)
    )
    forAll(testcases): (version1, version2, exp) =>
      assertResult(exp)(SAMProblems.compareVersion(version1, version2))

  test("1523. Count odd numbers in an interval range."):
    val testcases = Table(
      ("low", "high", "expected"),
      (3, 7, 3),
      (8, 10, 1)
    )
    forAll(testcases): (low, high, expected) =>
      assertResult(expected)(SAMProblems.countOdds(low, high))

  test("1491. Average salary excluding the minimum and maximum salary."):

    val testcases = Table(
      ("salaries", "expected average"),
      (List(4000, 3000, 1000, 2000), 2500.00000),
      (List(1000, 2000, 3000), 2000.00000),
      (
        List(48000, 59000, 99000, 13000, 78000, 45000, 31000, 17000, 39000, 37000, 93000, 77000,
          33000, 28000, 4000, 54000, 67000, 6000, 1000, 11000),
        41111.11111
      )
    )
    forAll(testcases): (salaries, exp) =>
      assert(exp +- 0.00001 === SAMProblems.average(salaries.toArray))

  test("881. Boats to save people."):
    val testcases = Table(
      ("people", "limit", "expected boats"),
      (List(1, 2), 3, 1),
      (List(3, 2, 2, 1), 3, 3),
      (List(3, 5, 3, 4), 5, 4)
    )
    forAll(testcases): (ppl, limit, exp) =>
      assertResult(exp)(SAMProblems.numRescueBoats(ppl.toArray, limit))

  test("2785. Sort vowel in a string."):
    val testcases = Table(
      ("string", "expected"),
      ("lEetcOde", "lEOtcede"),
      ("lYmpH", "lYmpH")
    )
    forAll(testcases): (string, exp) =>
      assertResult(exp)(SAMProblems.sortVowels(string))

  test("1929. Concatenation of array") {
    val testcases = Table(
      ("ints", "expected"),
      (List(1, 2, 1), List(1, 2, 1, 1, 2, 1)),
      (List(1, 3, 2, 1), List(1, 3, 2, 1, 1, 3, 2, 1))
    )
    forAll(testcases): (ints, exp) =>
      assertResult(exp)(SAMProblems.getConcatenation(ints.toArray).toList)
  }

  test("1512. Number of good pairs.") {
    val testcases = Table(
      ("ints", "exp"),
      (List(1, 2, 3, 1, 1, 3), 4),
      (List(1, 1, 1, 1), 6),
      (List(1, 2, 3), 0)
    )
    forAll(testcases): (ints, exp) =>
      assertResult(exp):
        SAMProblems.numIdenticalPairs(ints.toArray)
  }

  test("1470. Shuffle the array"):
    val testcases = Table(
      ("ints", "expected"),
      (List(2, 5, 1, 3, 4, 7), List(2, 3, 5, 4, 1, 7)),
      (List(1, 2, 3, 4, 4, 3, 2, 1), List(1, 4, 2, 3, 3, 2, 4, 1)),
      (List(1, 1, 2, 2), List(1, 2, 1, 2))
    )
    forAll(testcases): (ints, exp) =>
      assert(SAMProblems.shuffle(ints.toArray, ints.length / 2).toList === exp)

  test("1365. How many numbers are smaller than the current number."):
    val testcases = Table(
      ("ints", "exp"),
      (List(7, 7, 7, 7), List(0, 0, 0, 0)),
      (List(8, 1, 2, 2, 3), List(4, 0, 1, 1, 3)),
      (List(6, 5, 4, 8), List(2, 1, 0, 3))
    )
    forAll(testcases): (ints, exp) =>
      assert(SAMProblems.smallerNumbersThanCurrent(ints.toArray) === exp)

  test("506. Relative ranks."):
    val testcases = Table(
      ("score", "exp"),
      (List(5, 4, 3, 2, 1), List("Gold Medal", "Silver Medal", "Bronze Medal", "4", "5")),
      (List(10, 3, 8, 9, 4), List("Gold Medal", "5", "Bronze Medal", "Silver Medal", "4"))
    )
    forAll(testcases): (scores, exp) =>
      assertResult(exp):
        SAMProblems.findRelativeRanks(scores.toArray).toList

  test("2798. Number of employees who met the target."):
    val testcases = Table(
      ("hours", "target", "exp"),
      (List(0, 1, 2, 3, 4), 2, 3),
      (List(5, 1, 4, 2, 2), 6, 0)
    )
    forAll(testcases): (hours, target, exp) =>
      assert(SAMProblems.numberOfEmployeesWhoMetTarget(hours.toArray, target) === exp)

  test("2824. Count pairs whose sum is less than target"):
    val testcases = Table(
      ("ints", "target", "exp"),
      (List(-1, 1, 2, 3, 1), 2, 3),
      (List(-6, 2, 5, -2, -7, -1, 3), -2, 10)
    )
    forAll(testcases): (ints, target, exp) =>
      assertResult(exp):
        SAMProblems.countPairs(ints, target)

  test("1480. Running sum of 1d array."):
    val testcases = Table(
      ("ints", "exp"),
      (List(1, 2, 3, 4), List(1, 3, 6, 10)),
      (List(1, 1, 1, 1, 1), List(1, 2, 3, 4, 5)),
      (List(3, 1, 2, 10, 1), List(3, 4, 6, 16, 17))
    )
    forAll(testcases): (ints, exp) =>
      assertResult(exp):
        SAMProblems.runningSum(ints.toArray).toList
  test("1313. Decompress run length encoded list"):
    val testcases = Table(
      ("encoded list", "exp"),
      (List(1, 2, 3, 4), List(2, 4, 4, 4)),
      (List(1, 1, 2, 3), List(1, 3, 3))
    )
    forAll(testcases): (encodedlist, exp) =>
      assertResult(exp)(SAMProblems.decompressRLElist(encodedlist.toArray).toList)

  test("1389. Create target array in the given order."):
    val testcases = Table(
      ("ints", "index", "exp"),
      (List(0, 1, 2, 3, 4), List(0, 1, 2, 2, 1), List(0, 4, 1, 3, 2)),
      (List(1, 2, 3, 4, 0), List(0, 1, 2, 3, 0), List(0, 1, 2, 3, 4)),
      (List(1), List(0), List(1))
    )
    forAll(testcases): (ints, indices, exp) =>
      assertResult(exp):
        SAMProblems.createTargetArray(ints.toArray, indices.toArray).toList

  test("3075. Maximize happiness of selected children."):
    val testcases = Table(
      ("happiness", "k", "exp"),
      (List(1, 2, 3), 2, 4),
      (List(1, 1, 1, 1), 2, 1),
      (List(2, 3, 4, 5), 1, 5),
      (
        List(2135218, 73431904, 92495076, 77528042, 82824634, 3036629, 28375907, 65220365, 40948869,
          58914871, 57169530, 89783499, 19582915, 19676695, 11932465, 21770144, 49740276, 22303751,
          80746555, 97391584, 95775653, 43396943, 47271136, 43935930, 59643137, 64183008, 8892641,
          39587569, 85086654, 5663585, 82925096, 24868817, 95900395, 48155864, 74447380, 7618448,
          63299623, 91141186, 33347112, 81951555, 52867615, 92184410, 7024265, 85525916, 29846922,
          59532692, 47267934, 6514603, 1137830, 97807470),
        41,
        2517853814L
      )
    )
    forAll(testcases): (happiness, k, exp) =>
      assertResult(exp):
        SAMProblems.maximumHappinessSum(happiness.toArray, k)

  test("2574. Left and right sum differences."):
    val testcases = Table(
      ("nums", "exp"),
      (List(10, 4, 8, 3), List(15, 1, 11, 22)),
      (List(1), List(0))
    )
    forAll(testcases): (nums, exp) =>
      assertResult(exp):
        SAMProblems.leftRightDifference(nums.toArray).toList

  test("1732. Find the highest altitude."):
    val testcases = Table(
      ("gains", "exp"),
      (List(-5, 1, 5, 0, -7), 1),
      (List(-4, -3, -2, -1, 4, 3, 2), 0)
    )
    forAll(testcases): (gains, exp) =>
      assertResult(exp):
        SAMProblems.largestAltitude(gains.toArray)

  test("3065. Minimum number of operations to exceed threshold value."):
    val testcases = Table(
      ("nums", "k", "exp"),
      (List(2, 11, 10, 1, 3), 10, 3)
    )
    forAll(testcases): (nums, k, exp) =>
      assertResult(exp):
        SAMProblems.minOperations(nums.toArray, k)

  test("169. Majority element."):
    val testcases = Table(
      ("nums", "exp"),
      (List(3, 2, 3), 3),
      (List(2, 2, 1, 1, 1, 2, 2), 2)
    )
    forAll(testcases): (nums, exp) =>
      assertResult(exp):
        SAMProblems.majorityElement(nums.toArray)

  test("860. Lemonade change."):
    val testcases = Table(
      ("bills", "exp"),
      (List(5, 5, 5, 10, 20), true),
      (List(5, 5, 10, 10, 20), false)
    )
    forAll(testcases): (bills, exp) =>
      assert(SAMProblems.lemonadeChange(bills.toArray) === exp)

  test("976. Largest perimeter triangle."):
    val testcases = Table(
      ("ints", "exp perimeter"),
      (List(2, 1, 2), 5),
      (List(1, 2, 1, 10), 0),
      (List(3, 2, 3, 4), 10)
    )
    forAll(testcases): (ints, exp) =>
      assertResult(exp):
        SAMProblems.largestPerimeter(ints.toArray)

  test("1232. Check if is a straight line."):
    val testcases = Table(
      ("coordinates", "is straight line"),
      (List((1, 2), (2, 3), (3, 4), (4, 5), (5, 6), (6, 7)), true),
      (List((1, 1), (2, 2), (3, 4), (4, 5), (5, 6), (7, 7)), false)
    )
    forAll(testcases): (coords, isLine) =>
      val points = coords.map(t => Array(t._1, t._2)).toArray
      assertResult(isLine):
        SAMProblems.checkStraightLine(points)

  test("1588. Sum of all odd length subarrays."):
    val testcases = Table(
      ("ints", "expected"),
      (List(1, 4, 2, 5, 3), 58),
      (List(1, 2), 3),
      (List(10, 11, 12), 66)
    )
    forAll(testcases): (ints, exp) =>
      assertResult(exp):
        SAMProblems.sumOddLengthSubarrays(ints.toArray)

  test("1534. Count good triplets."):
    val testcases = Table(
      ("array", "a", "b", "c", "expected"),
      (List(3, 0, 1, 1, 9, 7), 7, 2, 3, 4)
      // (List(1, 1, 2, 2, 3), 0, 0, 1, 0)
    )

    forAll(testcases): (ints, a, b, c, expected) =>
      assertResult(expected)(SAMProblems.countGoodTriplets(ints.toArray, a, b, c))

  test("2956. Find common elements between two arrays."):
    val testcases = Table(
      ("array-1", "array-2", "exp"),
      (List(4, 3, 2, 3, 1), List(2, 2, 5, 2, 3, 6), List(3, 4)),
      (List(3, 4, 2, 3), List(1, 5), List(0, 0))
    )
    forAll(testcases): (a1, a2, exp) =>
      assertResult(exp):
        SAMProblems.findIntersectionValues(a1.toArray, a2.toArray).toList

  test("2006. Count number of pairs with absolute difference k."):
    val testcases = Table(
      ("ints", "k", "exp"),
      (List(1, 2, 2, 1), 1, 4),
      (List(1, 3), 3, 0),
      (List(3, 2, 1, 5, 4), 2, 3)
    )
    forAll(testcases): (ints, k, exp) =>
      assertResult(exp):
        SAMProblems.countKDifference(ints.toArray, k)

  test("1464. Maximum product of two elements in an array."):
    val testcases = Table(
      ("int", "exp"),
      (List(3, 4, 5, 2), 12),
      (List(1, 5, 4, 5), 20)
    )
    forAll(testcases): (ints, exp) =>
      assertResult(exp):
        SAMProblems.maxProduct(ints.toArray)

  test("1. Two Sum."):
    val testcases = Table(
      ("integers", "target", "expected"),
      (List(2, 7, 11, 15), 9, List(0, 1)),
      (List(3, 2, 4), 6, List(1, 2)),
      (List(3, 3), 6, List(0, 1))
    )
    forAll(testcases): (ints, target, exp) =>
      assert(SAMProblems.twoSum(ints.toArray, target).toList === exp)

  test("15. 3Sum."):
    val testcases = Table(
      ("ints", "exp"),
      (List(-1, 0, 1, 2, -1, -4), List(List(-1, -1, 2), List(-1, 0, 1))),
      (List(0, 0, 0), List(List(0, 0, 0))),
      (List(0, 1, 1), List.empty)
    )
    forAll(testcases): (ints, exp) =>
      val actuals = SAMProblems.threeSum(ints.toArray).toList
      assert(
        exp.forall(actuals.contains(_))
      )

  test("27. Remove element."):
    val testcases = Table(
      ("ints", "val", "expected array", "expected size"),
      (List(3, 2, 2, 3), 3, List(2, 2), 2),
      (List(0, 1, 2, 2, 3, 0, 4, 2), 2, List(0, 1, 4, 0, 3), 5),
      (List(1, 2, 3), 10, List(1, 2, 3), 3)
    )
    forAll(testcases): (ints, value, expArray, expSize) =>
      val actualSize = SAMProblems.removeElement(ints.toArray, value)
      assertResult(expSize)(actualSize)
      // assertResult(expArray)(ints.take(actualSize).toList)

  test("26. Remove duplicates from sorted array."):
    val testcases = Table(
      ("int", "exp"),
      (List(1, 1, 2), 2),
      (List(0, 0, 1, 1, 1, 2, 2, 3, 3, 4), 5)
    )
    forAll(testcases): (ints, exp) =>
      assertResult(exp):
        SAMProblems.removeDuplicates(ints.toArray)

  test("136. Single number"):
    val testcases = Table(
      ("ints", "exp"),
      (List(2,2,1), 1),
      (List(4,1,2,1,2), 4),
      (List(1), 1)
    )
    forAll(testcases): (ints, exp) =>
      assertResult(exp)(SAMProblems.singleNumber(ints.toArray))
