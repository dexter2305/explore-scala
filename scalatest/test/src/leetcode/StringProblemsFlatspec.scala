package leetcode

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalacheck.Gen

class StringProblemsFlatspec extends AnyFlatSpec with ScalaCheckPropertyChecks:
    "2114. Maximum number of words in a sentence" should "pass leetcode examples." in:
        val testdata = Table(
            ("sentences", "expected"),
            (Array("please wait", "continue to fight", "continue to win"), 3),
            (Array("alice and bob love leetcode", "i think so too", "this is great thanks very much"), 6),
            (Array("hello world", "unspoken truth of humanity"), 4)
        )
        forAll(testdata): (sentences, expected) =>
            assert(StringProblems.maxNumberOfWords(sentences) == expected)

    "1108. Defanging IP addresses" should "pass examples" in:
        val testdata = Table(
            ("given", "expected"),
            ("1.1.1.1", "1[.]1[.]1[.]1"),
            ("255.100.50.0", "255[.]100[.]50[.]0")
        )
        forAll(testdata): (input, expected) =>
            assert(StringProblems.defangingIPAddress(input) == expected)

    "709. To lowercase" should "pass random input from generator." in:
        forAll(Gen.alphaStr): (s: String) =>
            assert(StringProblems.toLowerCase(s) == s.toLowerCase())

    "2129. Capitalize the title" should "pass given examples." in:
        val testdata = Table(
            ("input", "expected"),
            ("This is a Valid Title", "This is a Valid Title"),
            ("capiTalIze tHe titLe", "Capitalize The Title"),
            ("First leTTeR of EACH Word", "First Letter of Each Word"),
            ("i lOve leetcode", "i Love Leetcode")
        )
        forAll(testdata): (input, expected) =>
            assert(StringProblems.capitalizeTheTitle(input) == expected)

    "520. Detect capital" should "pass basic examples." in:
        val testdata = Table(
            ("string", "expected"),
            ("INDIA", true),
            ("india", true),
            ("India", true),
            ("InDIa", false)
        )
        forAll(testdata): (input, expected) =>
            assert(StringProblems.detectCapital(input) == expected)

    "771. Jewels and stones" should "pass leetcode examples" in:
        val testdata = Table(
            ("jewels", "stones", "expected"),
            ("aA", "aAAbbbb", 3),
            ("z", "ZZ", 0)
        )
        forAll(testdata): (jewels, stones, expected) =>
            assert(StringProblems.jewelsAndStones(jewels, stones) == expected)

    "2011. Final value after performing operations" should "pass basic tests" in:
      val testcases = Table(
        ("ops", "expected"),
        (Array("--X","X++","X++"), 1),
        (Array("++X","++X","X++"), 3),
        (Array("X++","++X","--X","X--"), 0)
      )
      forAll(testcases): (operations, expected) =>
        assert(StringProblems.finalValueAfterOps(operations) == expected)

    "2942. Find words containing" should "pass basic tests" in:
      val testcases = Table(
        ("words", "character", "expectation"),
        (Array("leet","code"), 'e', Array(0,1)),
        (Array("abc","bcd","aaaa","cbc"), 'a', Array(0,2)),
        (Array("abc","bcd","aaaa","cbc"), 'z', Array.empty[Int])
      )
      forAll(testcases):(words, x, expected) =>
        assert(StringProblems.findWordsContaining(words, x).toList == expected.toList)

    "1662. Check if two strings are equivalent" should "pass basic tests" in:
      val testcases = Table(
        ("param#1", "param#2", "expected"),
        (Array("ab", "c"), Array("a", "bc"), true),
        (Array("a", "cb"), Array("ab", "c"), false),
        (Array("abc", "d", "defg"), Array("abcddefg"), true)
      )
      forAll(testcases):(word1, word2, expected) =>
        assert(StringProblems.arrayStringsAreEqual(word1, word2) == expected)

    "1816. Truncate sentence" should "pass simple tests" in:
      val testcases = Table(
        ("sentence", "length", "expected"),
        ("Hello how are you Contestant", 4, "Hello how are you"),
        ("What is the solution to this problem", 4, "What is the solution"),
        ("chopper is not a tanuki", 5,"chopper is not a tanuki")
      )
      forAll(testcases): (string, k, expected) =>
        assert(StringProblems.truncateSentence(string, k) == expected)


    "1773. Count items matching a rule" should "pass simple tests" in:
      val testcases = Table(
        ("items", "key", "value", "expected"),
        (List(List("phone","blue","pixel"),List("computer","silver","lenovo"),List("phone","gold","iphone")), "color", "silver", 1),
        (List(List("phone","blue","pixel"),List("computer","silver","phone"),List("phone","gold","iphone")), "type", "phone", 2)
      )
      forAll(testcases): (items, ruleKey, ruleValue, expected) =>
        assert(StringProblems.countMatches(items, ruleKey, ruleValue) == expected)

    "1832. Check if sentence is pangram" should "pass basic tests" in:
        val testcases = Table(
            ("sentence", "expected"),
            ("thequickbrownfoxjumpsoverthelazydog", true),
            ("leetcode", false)
        )
        forAll(testcases): (input, expected) =>
            assert(StringProblems.checkIfPangram(input) == expected)

    "557. Reverse words III" should "pass basic tests" in:
        val testcases = Table(
            ("input", "expected"),
            ("Let's take LeetCode contest", "s'teL ekat edoCteeL tsetnoc"),
            ("Mr Ding", "rM gniD")
        )
        forAll(testcases): (input, expected) =>
            assert(StringProblems.reverseWords(input) == expected)

    "1859. Sorting the sequence" should "pass basic tests" in:
        val testcases = Table(
            ("input", "expected"),
            ("Myself2 Me1 I4 and3", "Me Myself and I"),
            ("is2 sentence4 This1 a3", "This is a sentence")
        )
        forAll(testcases): (input, expected) =>
            assert(StringProblems.sortSentence(input) == expected)

    "2810. Faulty keyboard" should "pass basic tests" in:
        val testcases = Table(
            ("input", "expected"),
            ("string", "rtsng"),
            ("poiinter", "ponter")
        )
        forAll(testcases): (input, expected) =>
            assert(StringProblems.finalString(input) == expected)

    "541. Reverse String II" should "pass basic tests" in:
            val testdata = Table(
                ("string", "k", "expected"),
                ("abcdefg", 2, "bacdfeg"),
                ("abcd",2, "bacd")
            )
            forAll(testdata): (input, k, expected) =>
                assert(StringProblems.reverseStr(input, k) == expected)

    "2828. Check if a string is an acronym of words" should "pass basic tests" in:
            val testcases = Table(
                ("strings", "acronym", "expected"),
                (List("alice","bob","charlie"), "abc", true),
                (List("an","apple"), "a", false),
                (List("never","gonna","give","up","on","you"), "ngguoy", true)
            )
            forAll(testcases): (strings, acronym, expected) =>
                assert(StringProblems.isAcronym(strings, acronym) == expected)

    "1684. Count the number of consistent strings" should "pass basic tests" in:
            val testcases = Table(
                ("strings", "dictionary", "count"),
                (Array("ad","bd","aaab","baa","badab"), "ab", 2),
                (Array("a","b","c","ab","ac","bc","abc"), "abc", 7),
                (Array("cc","acd","b","ba","bac","bad","ac","d"), "cad", 4)
            )
            forAll(testcases): (strings, dictionary, expected) =>
                assert(StringProblems.countConsistentStrings(dictionary, strings) == expected)

    "2418. Sort the people" should "pass basic tests" in:
        val testcases = Table(
            ("user name list", "user age list", "expected"),
            (Array("Mary","John","Emma"), Array(180,165,170), Array("Mary","Emma","John")),
            (Array("Alice","Bob","Bob"), Array(155,185,150), Array("Bob","Alice","Bob"))
        )
        forAll(testcases): (names, heights, expected) =>
            assert(StringProblems.sortPeople(names, heights).toList == expected.toList)

    "917. Reverse only letters" should "pass basic tests" in:
            val testcases = Table(
                ("ab-cd", "dc-ba"),
                ("a-bC-dEf-ghIj", "j-Ih-gfE-dCba"),
                ("Test1ng-Leet=code-Q!","Qedo1ct-eeLg=ntse-T!")
            )
            forAll(testcases): (input, expected) =>
                assert(StringProblems.reverseOnlyLetters(input) == expected)

    "344. Reverse string" should "pass basic tests" in:
        val data = "Hello".toCharArray()
        StringProblems.reverseString(data)
        assert(data.toList.equals("olleH".toCharArray().toList))
