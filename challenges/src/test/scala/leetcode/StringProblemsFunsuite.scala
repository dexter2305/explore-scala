package leetcode

import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class StringProblemsFunsuite extends AnyFunSuite with ScalaCheckPropertyChecks:

  test("2114. Maximum number of words in a sentence"):
    val testcases = List(
      (Array("please wait", "continue to fight", "continue to win"), 3),
      (Array("alice and bob love leetcode", "i think so too", "this is great thanks very much"), 6),
      (Array("hello world", "unspoken truth of humanity"), 4)
    )
    for testcase <- testcases do
      testcase match
        case (input, expected) => assert(StringProblems.maxNumberOfWords(input) === expected)

  test("1108. Defanging IP address"):
    val testCases = List(
      ("1.1.1.1", "1[.]1[.]1[.]1"),
      ("255.100.50.0", "255[.]100[.]50[.]0")
    )
    for (input, expected) <- testCases do
      assert(StringProblems.defangingIPAddress(input) === expected)

  test("709. To Lowercase"):
    val testcases = List(
      ("Must BE LOweR", "must be lower")
    )
    for (input, e) <- testcases do assert(StringProblems.toLowerCase(input) === e)

  test("2129. Capitalize the title"):
    val testcases = List(
      ("This is a Valid Title", "This is a Valid Title"),
      ("capiTalIze tHe titLe", "Capitalize The Title"),
      ("First leTTeR of EACH Word", "First Letter of Each Word"),
      ("i lOve leetcode", "i Love Leetcode")
    )
    for (input, expected) <- testcases do
      assert(StringProblems.capitalizeTheTitle(input) === expected)

  test("520. Detect capital"):
    val testcases = List(
      ("INDIA", true),
      ("india", true),
      ("India", true),
      ("InDIa", false)
    )
    for (input, expected) <- testcases do assert(StringProblems.detectCapital(input) === expected)

  test("771. Jewels and stones"):
    val testcases = List(
      (("aA", "aAAbbbb"), 3),
      (("z", "ZZ"), 0)
    )
    for ((jewels, stones), expected) <- testcases do
      assert(
        StringProblems.jewelsAndStones(jewels, stones) === expected,
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
        StringProblems.finalValueAfterOps(input) === expected,
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
        StringProblems.arrayStringsAreEqual(word1, word2) === expected,
        clue = s"Expecting equivalence between word1:'${word1.mkString(",")}' & word2:'${word2
            .mkString(",")}' as '$expected'"
      )

  test("1816. Truncate sentence"):
    val testcases = List(
      ("Hello how are you Contestant", 4, "Hello how are you"),
      ("What is the solution to this problem", 4, "What is the solution"),
      ("chopper is not a tanuki", 5, "chopper is not a tanuki")
    )
    for (string, k, expected) <- testcases do
      assert(StringProblems.truncateSentence(string, k) === expected)

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
      assert(StringProblems.countMatches(items, ruleKey, ruleValue) === expected)

  test("1832. Check if sentence is pangram"):
    val testcases = List(
      ("thequickbrownfoxjumpsoverthelazydog", true),
      ("leetcode", false)
    )
    for (input, expected) <- testcases do assert(StringProblems.checkIfPangram(input) === expected)

  test("557. Reverse words III"):
    val testcases = List(
      ("Let's take LeetCode contest", "s'teL ekat edoCteeL tsetnoc"),
      ("Mr Ding", "rM gniD")
    )
    for (input, expected) <- testcases do assert(StringProblems.reverseWords(input) === expected)

  test("1859. Sorting the sequence"):
    val testcases = List(
      ("Myself2 Me1 I4 and3", "Me Myself and I"),
      ("is2 sentence4 This1 a3", "This is a sentence")
    )
    for (input, expected) <- testcases do assert(StringProblems.sortSentence(input) === expected)

  test("2810. Faulty keyboard"):
    val testcases = List(
      ("string", "rtsng"),
      ("poiinter", "ponter")
    )
    for (input, expected) <- testcases do assert(StringProblems.finalString(input) === expected)

  test("541. Reverse String II"):
    for (string, k, expected) <-
        List(
          ("abcdefg", 2, "bacdfeg"),
          ("abcd", 2, "bacd")
        )
    do assert(StringProblems.reverseStr(string, k) === expected)

  test("2828. Check if a string is an acronym of words"):
    for (words, s, expected) <- List(
        (List("alice", "bob", "charlie"), "abc", true),
        (List("an", "apple"), "a", false),
        (List("never", "gonna", "give", "up", "on", "you"), "ngguoy", true)
      )
    do assert(StringProblems.isAcronym(words, s) === expected)

  test("1684. Count the number of consistent strings"):
    for (words, allowed, expected) <- List(
        (Array("ad", "bd", "aaab", "baa", "badab"), "ab", 2),
        (Array("a", "b", "c", "ab", "ac", "bc", "abc"), "abc", 7),
        (Array("cc", "acd", "b", "ba", "bac", "bad", "ac", "d"), "cad", 4)
      )
    do
      assert(
        StringProblems.countConsistentStrings(allowed, words) === expected,
        clue = s"Expecting '[${words.mkString(",")}]' to contain '${allowed}' '$expected' times."
      )

  test("2418. Sort the people"):
    val testcases = List(
      (Array("Mary", "John", "Emma"), Array(180, 165, 170), Array("Mary", "Emma", "John")),
      (Array("Alice", "Bob", "Bob"), Array(155, 185, 150), Array("Bob", "Alice", "Bob"))
    )
    for (names, heights, expected) <- testcases do
      assert(StringProblems.sortPeople(names, heights).toList === expected.toList)

  test("917. Reverse only letters"):
    for (input, expected) <- List(
        ("ab-cd", "dc-ba"),
        ("a-bC-dEf-ghIj", "j-Ih-gfE-dCba"),
        ("Test1ng-Leet=code-Q!", "Qedo1ct-eeLg=ntse-T!")
      )
    do assert(StringProblems.reverseOnlyLetters(input) === expected)

  test("344. Reverse string"):
    val data = "Hello".toCharArray()
    StringProblems.reverseString(data)
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
    for (input, expected) <- testcases do assert(StringProblems.destCity(input) === expected)

  test("383. Can construct"):
    val testcases = List(
      ("a", "b", false),
      ("aa", "ab", false),
      ("aa", "aab", true)
    )

    for (ransomeNote, magazine, expected) <- testcases do
      assert(
        StringProblems.canConstruct(ransomeNote, magazine) === expected,
        clue = s"Can construct $ransomeNote from $magazine should be $expected"
      )

  test("2047. Number of valid words in a sentence"):
    // pendingUntilFixed:
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
        StringProblems.countValidWords(testcase) === expected,
        clue = s"Expected '${testcase}' to return '$expected'"
      )

  test("2138. Divide a string into groups of size k"):
    val testcases = Table(
      ("s", "k", "fill", "expected"),
      ("abcdefghi", 3, 'x', Array("abc", "def", "ghi")),
      ("abcdefghij", 3, 'x', Array("abc", "def", "ghi", "jxx"))
    )
    forAll(testcases): (s, k, fill, expected) =>
      assert(StringProblems.divideString(s, k, fill).toList === expected.toList)

  test("2108. Find first palindromic string in the array"):
    val testcases = Table(
      ("given-array", "expected"),
      (Array("abc", "car", "ada", "racecar", "cool"), "ada"),
      (Array("notapalindrome", "racecar"), "racecar"),
      (Array("def", "ghi"), "")
    )
    forAll(testcases): (givenArray, expected) =>
      assert(
        StringProblems.firstPalindrome(givenArray) === expected,
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
        StringProblems.isPalindrome(string) === expected,
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
        StringProblems.countSegments(string) === expected,
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
      assert(StringProblems.mergeAlternately(word1, word2) === expected)

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
        StringProblems.maxDepth(string) === expectation,
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
      assert(StringProblems.removeTrailingZeros(num) === expected)

  test("2678. Number of senior citizens"):
    val testcases = Table(
      ("details", "expected"),
      (Array("7868190130M7522", "5303914400F9211", "9273338290F4010"), 2),
      (Array("1313579440F2036", "2921522980M5644"), 0)
    )
    forAll(testcases): (details, expected) =>
      assert(StringProblems.countSeniors(details) === expected)

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
      assert(StringProblems.makeGood(s) === expected)

  test("58. Length of last word"):
    val testcases = Table(
      ("sentence", "expected"),
      ("Hello World", 5),
      ("   fly me   to   the moon  ", 4),
      ("luffy is still joyboy", 6),
      ("a", 1),
      ("", 0)
    )
    forAll(testcases): (sentence, expected) =>
      assert(StringProblems.lengthOfLastWord(sentence) === expected)

  test("28. Find index of the first occurence in a string"):
    val testcases = Table(
      ("haystack", "needle", "expected"),
      ("sadbutsad", "sad", 0),
      ("leetcode", "leeto", -1)
    )
    forAll(testcases): (haystack, needle, expected) =>
      assert(StringProblems.strStr(haystack, needle) === expected)

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
      assert(StringProblems.isValid(input) === expected)

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
      assert(StringProblems.minRemoveToMakeValid(string) === expected)

  test("1678. Goal parser interpretation"):
    val testcases = Table(
      ("command", "expected"),
      ("G()(al)", "Goal"),
      ("G()()()()(al)", "Gooooal"),
      ("(al)G(al)()()G", "alGalooG")
    )
    forAll(testcases): (command, expected) =>
      assert(StringProblems.interpret(command) === expected)

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
        assert(StringProblems.checkValidString(string) === expected)

  test("389. Find the difference"):
    val testcases = Table(
      ("s", "t", "appended char"),
      ("abcd", "abcde", 'e'),
      ("", "y", 'y')
    )
    forAll(testcases): (s, t, expected) =>
      assert(StringProblems.findTheDifference(s, t) == expected)

  test("2000. Reverse prefix of word"):
    val testcases = Table(
      ("word", "ch", "expected"),
      ("abcdefd", 'd', "dcbaefd"),
      ("xyxzxe", 'z', "zxyxxe"),
      ("abcd", 'z', "abcd"),
      ("abcd", 'd', "dcba")
    )
    forAll(testcases): (word, ch, expected) =>
      assert(StringProblems.reversePrefix(word, ch) === expected)

  test("3019. Number of changing keys"):
    val testcases = Table(
      ("string", "expected"),
      ("aAbBcC", 2),
      ("AaAaAaaA", 0),
      ("a", 0)
    )
    forAll(testcases): (string, expected) =>
      assert(StringProblems.countKeyChanges(string) === expected)

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
      assert(StringProblems.balancedStringSplit(s) === expected)

  test("1528. Shufftle string"):
    val testcases = Table(
      ("s", "indices", "expected"),
      ("codeleet", Array(4, 5, 6, 7, 0, 2, 1, 3), "leetcode"),
      ("abc", Array(0, 1, 2), "abc")
    )
    forAll(testcases): (string, array, expected) =>
      assert(StringProblems.restoreString(string, array) === expected)

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
      assert(StringProblems.decodeMessage(key, message) === expectedDecodedMessage)

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
      assert(StringProblems.addBinary(x, y) === expected)

    // property based test to cover wider range
    given PropertyCheckConfiguration(minSuccessful = 50000, maxDiscardedFactor = 100)
    val intGenerator = for n <- Gen.choose(0, Int.MaxValue) yield n
    forAll((intGenerator, "a"), (intGenerator, "b")): (a: Int, b: Int) =>
      whenever(a >= 0 && a < Int.MaxValue && b >= 0 && b != Int.MinValue):
        val expectedString = (a + b).toBinaryString
        assert(
          StringProblems.addBinary(a.toBinaryString, b.toBinaryString) === (a + b).toBinaryString
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
      assert(StringProblems.addStrings(a, b) === expected)

    given PropertyCheckConfiguration(minSuccessful = 1000, maxDiscardedFactor = 1)
    // property based tests
    val intGenerator = for n <- Gen.choose(0, 9999) yield n

    forAll((intGenerator, "a"), (intGenerator, "b")): (a: Int, b: Int) =>
      whenever(a > 0 && a < Int.MaxValue && b > 0 && b < Int.MaxValue):
        assert(StringProblems.addStrings(a.toString(), b.toString) === (a + b).toString)

  test("2864. Maximum odd binary number"):
    val testcases = Table(
      ("010", "001"),
      ("0101", "1001"),
      ("1", "1")
    )
    forAll(testcases): (string, expected) =>
      assert(StringProblems.maximumOddBinaryNumber(string) === expected)

  test("804. Unique morse code of words."):
    val testcases = Table(
      ("words", "expeced unique transformations"),
      (List("gin", "zen", "gig", "msg"), 2),
      (List("a"), 1)
    )
    forAll(testcases): (words, expected) =>
      assert(StringProblems.uniqueMorseRepresentations(words.toArray) === expected)

  test("242. Valid anagram."):
    val testcases = Table(
      ("s", "t", "anagram"),
      ("anagram", "nagaram", true),
      ("rat", "car", false)
    )
    forAll(testcases): (s, t, expected) =>
      assert(StringProblems.isAnagram(s, t) === expected)

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
      assert(StringProblems.isIsomorphic(s, t) === expected)

  test("412. Fizz Buzz"):
    val testcases = Table(
      ("n", "expected"),
      (1, List("1")),
      (2, List("1", "2")),
      (3, List("1", "2", "Fizz")),
      (5, List("1", "2", "Fizz", "4", "Buzz"))
    )
    forAll(testcases): (input, expected) =>
      assert(StringProblems.fizzBuzz(input) === expected)

  test("387. First unique character in a string."):
    val testcases = Table(
      ("string", "expected index"),
      ("leetcode", 0),
      ("loveleetcode", 2),
      ("aabb", -1)
    )
    forAll(testcases): (string, expectedIndex) =>
      assert(StringProblems.firstUniqChar(string) === expectedIndex)

  test("1556. Thousand separator."):
    val testcases = Table(
      ("n", "expected"),
      (987, "987"),
      (1000, "1.000")
    )
    forAll(testcases): (n, expected) =>
      assert(StringProblems.thousandSeparator(n) === expected)

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
      assert(StringProblems.numDifferentIntegers(string) === expected)

  test("1455. Check if a word occurs as a prefix of a word in a sentence."):
    val testcases = Table(
      ("sentence", "searchWord", "expectedIndex"),
      ("i love eating burger", "burg", 4),
      ("this problem is an easy problem", "pro", 2),
      ("i am tired", "you", -1),
      ("trick", "t", 1)
    )
    forAll(testcases): (sentence, searchWord, expectedIndex) =>
      assert(StringProblems.isPrefixOfWord(sentence = sentence, searchWord) === expectedIndex)

  test("2185. Counting words with a given prefix"):
    val testcases = Table(
      ("words", "prefix", "expected"),
      (List("pay", "attention", "practice", "attend"), "at", 2),
      (List("leetcode", "win", "loops", "success"), "code", 0)
    )
    forAll(testcases): (words, prefix, expected) =>
      assert(StringProblems.prefixCount(words.toArray, prefix) === expected)

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
      assert(StringProblems.checkString(string) == expected)

  test("500. Keyboard row"):
    val testcases = Table(
      ("words", "expected"),
      (List("Hello", "Alaska", "Dad", "Peace"), List("Alaska", "Dad")),
      (List("omk"), List.empty[String]),
      (List("adsdf", "sfd"), List("adsdf", "sfd"))
    )
    forAll(testcases): (words, expected) =>
      assert(StringProblems.findWords(words.toArray).toList === expected)

  test("2423. Remove letter to equalize frequency"):
    pendingUntilFixed:
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
        assert(StringProblems.equalFrequency(string) === expected)

  test("3110. Score of a string"):
    val testcases = Table(
      ("string", "expected score"),
      ("hello", 13),
      ("zaz", 50)
    )
    forAll(testcases): (string, expectedScore) =>
      assert(StringProblems.scoreOfString(string) === expectedScore)

  test("2194. Cells in a range on an excel sheet."):
    val testcases = Table(
      ("s", "expected range"),
      ("K1:L2", List("K1", "K2", "L1", "L2")),
      ("A1:F1", List("A1", "B1", "C1", "D1", "E1", "F1"))
    )
    forAll(testcases): (s, expected) =>
      assert(StringProblems.cellsInRange(s) === expected)

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
      assert(StringProblems.toGoatLatin(s) === expected)

  test("1002. Find common characters."):
    val testcases = Table(
      ("words", "expected common chars"),
      (List("bella", "label", "roller"), List("e", "l", "l")),
      (List("cool", "lock", "cook"), List("c", "o")),
      (List("a", "aaa"), List("a"))
    )
    forAll(testcases): (words, expected) =>
      assert(StringProblems.commonChars(words.toArray).sorted === expected.sorted)

  test("2716. Minimize string length."):
    val testcases = Table(
      ("string", "expected length"),
      ("aaabc", 3),
      ("cbbd", 3),
      ("dddaaa", 2)
    )
    forAll(testcases): (string, expectedLength) =>
      assert(StringProblems.minimizedStringLength(string) === expectedLength)

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
      assert(StringProblems.freqAlphabets(string) === expected)

  test("1844. Replace all digits with characters"):
    val testcases = Table(
      ("s", "expected"),
      ("a1c1e1", "abcdef"),
      ("a1b2c3d4e", "abbdcfdhe"),
      ("a", "a"),
      ("a3c", "adc")
    )
    forAll(testcases): (string, expected) =>
      assert(StringProblems.replaceDigits(string) === expected)

  test("2744. Find the maximum number of string pairs"):
    val testcases = Table(
      ("words", "expected pairs"),
      (List("cd", "ac", "dc", "ca", "zz"), 2),
      (List("ab", "ba", "cc"), 1),
      (List("aa", "ab"), 0)
    )
    forAll(testcases): (words, expected) =>
      assert(StringProblems.maximumNumberOfStringPairs(words.toArray) === expected)

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
      (roman, expected) => assert(StringProblems.romanToInt(roman) === expected)

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
      if !expectation then assert(StringProblems.reformat(string) === "")
      else
        val actual = StringProblems.reformat(string)
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
      assert(StringProblems.reverseVowels(givenString) === expected)

  test("1941. Check if all characters have equal occurences."):
    val testcases = Table(
      ("string", "expected"),
      ("abacbc", true),
      ("aaabb", false),
      ("a", true)
    )
    forAll(testcases): (s, expected) =>
      assert(StringProblems.areOccurrencesEqual(s) === expected)

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
      assert(StringProblems.countAsterisks(s) === expected)

  test("2255. Count prefixes of a given string"):
    val testcases = Table(
      ("words", "s", "expected"),
      (List("a", "b", "c", "ab", "bc", "abc"), "abc", 3),
      (List("a", "a"), "aa", 2)
    )
    forAll(testcases): (words, s, expected) =>
      assert(StringProblems.countPrefixes(words.toArray, s) === expected)

  test("2278. Percentage of letter in string"):
    val testcases = Table(
      ("s", "letter", "expected"),
      ("foobar", 'o', 33),
      ("jjjj", 'k', 0)
    )
    forAll(testcases): (s, letter, expected) =>
      assert(StringProblems.percentageLetter(s, letter) === expected)
