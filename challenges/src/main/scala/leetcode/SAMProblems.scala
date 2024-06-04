package leetcode

/** SAM Problems
  * String, Array, Math topics
  */
object SAMProblems:

  /** 2114. Maximum number of words found in sentences.
    *
    * A sentence is a list of words that are separated by a single space with no leading or trailing
    * spaces.
    *
    * You are given an array of strings sentences, where each sentences[i] represents a single
    * sentence.
    *
    * Return the maximum number of words that appear in a single sentence.
    *
    * Example 1:
    *
    * - Input: sentences = ["alice and bob love leetcode", "i think so too", "this is great thanks
    * very much"]
    * - Output: 6
    * - Explanation:
    *   - The first sentence, "alice and bob love leetcode", has 5 words in total.
    *   - The second sentence, "i think so too", has 4 words in total.
    *   - The third sentence, "this is great thanks very much", has 6 words in total. Thus, the maximum number of words in a single sentence comes from the third sentence, which has 6 words.
    *
    * Example 2:
    *
    * - Input: sentences = ["please wait", "continue to fight", "continue to win"]
    * - Output: 3
    * - Explanation: It is possible that multiple sentences contain the same number of words. In this example, the second and third sentences (underlined) have the same number of words.
    *
    * Constraints:
    *
    * - 1 <= sentences.length <= 100
    * - 1 <= sentences[i].length <= 100
    * - sentences[i] consists only of lowercase English letters and ' ' only.
    * - sentences[i] does not have leading or trailing spaces.
    * All the words in sentences[i] are separated by a single space.
    */
  def maxNumberOfWords(sentences: Array[String]): Int =
    sentences.map(_.count(_ == ' ') + 1).max

  /** 1108. Defanging an IP address
    *
    * Given a valid (IPv4) IP address, return a defanged version of that IP address.
    *
    * A defanged IP address replaces every period "." with "[.]".
    *
    * Example 1:
    *
    * Input: address = "1.1.1.1" Output: "1[.]1[.]1[.]1"
    *
    * Example 2:
    *
    * Input: address = "255.100.50.0" Output: "255[.]100[.]50[.]0"
    *
    * Constraints:
    *
    * The given address is a valid IPv4 address.
    */
  def defangingIPAddress(address: String): String =
    address.split("\\.").mkString("[.]")

  /** 709. To Lower case.
    *
    * Given a string s, return the string after replacing every uppercase letter with the same
    * lowercase letter.
    *
    * Example 1:
    *
    *   - Input: s = "Hello"
    *   - Output: "hello"
    *
    * Example 2:
    *
    *   - Input: s = "here"
    *   - Output: "here"
    *
    * Example 3:
    *
    *   - Input: s = "LOVELY"
    *   - Output: "lovely"
    *
    * Constraints:
    *
    *   - 1 <= s.length <= 100
    *   - s consists of printable ASCII characters.
    *
    * @note
    *   programming-skills
    */
  def toLowerCase(s: String): String =
    s.map: c =>
      if c >= 'A' && c <= 'Z' then (c + 32).toChar else c

  /** 2129. Capitalize the title.
    *
    * You are given a string title consisting of one or more words separated by a single space,
    * where each word consists of English letters. Capitalize the string by changing the
    * capitalization of each word such that:
    *
    * If the length of the word is 1 or 2 letters, change all letters to lowercase. Otherwise,
    * change the first letter to uppercase and the remaining letters to lowercase.
    *
    * Return the capitalized title.
    *
    * Example 1:
    *
    * Input: title = "capiTalIze tHe titLe" Output: "Capitalize The Title" Explanation: Since all
    * the words have a length of at least 3, the first letter of each word is uppercase, and the
    * remaining letters are lowercase.
    *
    * Example 2:
    *
    * Input: title = "First leTTeR of EACH Word" Output: "First Letter of Each Word" Explanation:
    * The word "of" has length 2, so it is all lowercase. The remaining words have a length of at
    * least 3, so the first letter of each remaining word is uppercase, and the remaining letters
    * are lowercase.
    *
    * Example 3:
    *
    * Input: title = "i lOve leetcode" Output: "i Love Leetcode" Explanation: The word "i" has
    * length 1, so it is lowercase. The remaining words have a length of at least 3, so the first
    * letter of each remaining word is uppercase, and the remaining letters are lowercase.
    *
    * Constraints:
    *
    * 1 <= title.length <= 100 title consists of words separated by a single space without any
    * leading or trailing spaces. Each word consists of uppercase and lowercase English letters and
    * is non-empty.
    */
  def capitalizeTheTitle(title: String): String =
    title
      .split(' ')
      .map: s =>
        if s.length() <= 2 then s.toLowerCase()
        else s"${s.head.toUpper}${s.tail.toLowerCase()}"
      .mkString(" ")

  /** 520. Detect capital.
    *
    * We define the usage of capitals in a word to be right when one of the following cases holds:
    *
    * All letters in this word are capitals, like "USA". All letters in this word are not capitals,
    * like "leetcode". Only the first letter in this word is capital, like "Google".
    *
    * Given a string word, return true if the usage of capitals in it is right.
    *
    * Example 1:
    *
    * - Input: word = "USA"
    * - Output: true
    *
    * Example 2:
    *
    * - Input: word = "FlaG"
    * - Output: false
    *
    * Constraints:
    *
    * - 1 <= word.length <= 100
    * - word consists of lowercase and uppercase English letters.
    */
  def detectCapital(capital: String): Boolean =
    capital.forall(_.isUpper) ||
      capital.forall(_.isLower) ||
      capital.head.isUpper && capital.tail.forall(_.isLower)

  /** 771. Jewels and stones.
    *
    * You're given strings jewels representing the types of stones that are jewels, and stones
    * representing the stones you have. Each character in stones is a type of stone you have. You
    * want to know how many of the stones you have are also jewels.
    *
    * Letters are case sensitive, so "a" is considered a different type of stone from "A".
    *
    * Example 1:
    *
    * - Input: jewels = "aA", stones = "aAAbbbb"
    * - Output: 3
    *
    * Example 2:
    *
    * - Input: jewels = "z", stones = "ZZ"
    * - Output: 0
    *
    * Constraints:
    *
    * - 1 <= jewels.length, stones.length <= 50
    * - jewels and stones consist of only English letters. All the characters of jewels are unique.
    */
  def jewelsAndStones(jewels: String, stones: String): Int =
    val j = jewels.toSet
    stones.count(j.contains)

  /** 2011. Final value after performing operations.
    *
    * There is a programming language with only four operations and one variable X:
    *
    * ++X and X++ increments the value of the variable X by 1. --X and X-- decrements the value of
    * the variable X by 1.
    *
    * Initially, the value of X is 0.
    *
    * Given an array of strings operations containing a list of operations, return the final value
    * of X after performing all the operations.
    *
    * Example 1:
    *
    * - Input: operations = ["--X","X++","X++"]
    * - Output: 1
    * - Explanation: The operations are performed as follows: Initially, X = 0. --X: X is decremented by 1, X = 0 - 1 = -1. X++: X is incremented by 1, X = -1 + 1 = 0. X++: X is incremented by 1, X = 0 + 1 = 1.
    *
    * Example 2:
    *
    * - Input: operations = ["++X","++X","X++"]
    * - Output: 3
    * - Explanation: The operations are performed as follows:
    *  - Initially, X = 0.
    *  - ++X: X is incremented by 1, X = 0 + 1 = 1.
    *  - ++X: X is incremented by 1, X = 1 + 1 = 2.
    *  - X++: X is incremented by 1, X = 2 + 1 = 3.
    *
    * Example 3:
    *
    * Input: operations = ["X++","++X","--X","X--"] Output: 0 Explanation: The operations are
    * performed as follows: Initially, X = 0. X++: X is incremented by 1, X = 0 + 1 = 1.
    * ++X: X is incremented by 1, X = 1 + 1 = 2. --X: X is decremented by 1, X = 2 - 1 = 1. X--: X
    * is decremented by 1, X = 1 - 1 = 0.
    *
    * Constraints:
    *
    * 1 <= operations.length <= 100 operations[i] will be either "++X", "X++", "--X", or "X--".
    */
  def finalValueAfterOps(ops: Array[String]): Int =
    ops.foldLeft(0): (acc, e) =>
      e match
        case "++X" | "X++" => acc + 1
        case "--X" | "X--" => acc - 1

  /** 2942. Find words containing character.
    *
    * You are given a 0-indexed array of strings words and a character x.
    *
    * Return an array of indices representing the words that contain the character x.
    *
    * Note that the returned array may be in any order.
    *
    * Example 1:
    *
    * Input: words = ["leet","code"], x = "e" Output: [0,1] Explanation: "e" occurs in both words:
    * "leet", and "code". Hence, we return indices 0 and 1.
    *
    * Example 2:
    *
    * Input: words = ["abc","bcd","aaaa","cbc"], x = "a" Output: [0,2] Explanation: "a" occurs in
    * "abc", and "aaaa". Hence, we return indices 0 and 2.
    *
    * Example 3:
    *
    * Input: words = ["abc","bcd","aaaa","cbc"], x = "z" Output: [] Explanation: "z" does not occur
    * in any of the words. Hence, we return an empty array.
    *
    * Constraints:
    *
    * 1 <= words.length <= 50 1 <= words[i].length <= 50 x is a lowercase English letter. words[i]
    * consists only of lowercase English letters.
    */
  def findWordsContaining(words: Array[String], x: Char): List[Int] =
    words.zipWithIndex
      .collect:
        case (word, index) if word contains (x) => index
      .toList

  /** 1662. Check if two strings are equivalent.
    *
    * Given two string arrays word1 and word2, return true if the two arrays represent the same
    * string, and false otherwise.
    *
    * A string is represented by an array if the array elements concatenated in order forms the
    * string.
    *
    * Example 1:
    *
    * Input: word1 = ["ab", "c"], word2 = ["a", "bc"] Output: true Explanation: word1 represents
    * string "ab" + "c" -> "abc" word2 represents string "a" + "bc" -> "abc" The strings are the
    * same, so return true.
    *
    * Example 2:
    *
    * Input: word1 = ["a", "cb"], word2 = ["ab", "c"] Output: false
    *
    * Example 3:
    *
    * Input: word1 = ["abc", "d", "defg"], word2 = ["abcddefg"] Output: true
    *
    * Constraints:
    *
    * 1 <= word1.length, word2.length <= 103 1 <= word1[i].length, word2[i].length <= 103 1 <=
    * sum(word1[i].length), sum(word2[i].length) <= 103 word1[i] and word2[i] consist of lowercase
    * letters.
    */
  def arrayStringsAreEqual(word1: Array[String], word2: Array[String]): Boolean =
    word1.mkString == word2.mkString

  /** 1816. Truncate sentence.
    *
    * A sentence is a list of words that are separated by a single space with no leading or trailing
    * spaces. Each of the words consists of only uppercase and lowercase English letters (no
    * punctuation).
    *
    * For example, "Hello World", "HELLO", and "hello world hello world" are all sentences.
    *
    * You are given a sentence s​​​​​​ and an integer k​​​​​​. You want to truncate s​​​​​​ such
    * that it contains only the first k​​​​​​ words. Return s​​​​​​ after truncating it.
    *
    * Example 1:
    *
    * Input: s = "Hello how are you Contestant", k = 4 Output: "Hello how are you" Explanation: The
    * words in s are ["Hello", "how" "are", "you", "Contestant"]. The first 4 words are ["Hello",
    * "how", "are", "you"]. Hence, you should return "Hello how are you".
    *
    * Example 2:
    *
    * Input: s = "What is the solution to this problem", k = 4 Output: "What is the solution"
    * Explanation: The words in s are ["What", "is" "the", "solution", "to", "this", "problem"]. The
    * first 4 words are ["What", "is", "the", "solution"]. Hence, you should return "What is the
    * solution".
    *
    * Example 3:
    *
    * Input: s = "chopper is not a tanuki", k = 5 Output: "chopper is not a tanuki"
    *
    * Constraints:
    *
    * 1 <= s.length <= 500 k is in the range [1, the number of words in s]. s consist of only
    * lowercase and uppercase English letters and spaces. The words in s are separated by a single
    * space. There are no leading or trailing spaces.
    */
  def truncateSentence(s: String, k: Int): String = s.split(" ").take(k).mkString(" ")

  /** 1773. Count items matching a rule.
    *
    * You are given an array items, where each items[i] = [typei, colori, namei] describes the type,
    * color, and name of the ith item. You are also given a rule represented by two strings, ruleKey
    * and ruleValue.
    *
    * The ith item is said to match the rule if one of the following is true:
    *
    * ruleKey == "type" and ruleValue == typei. ruleKey == "color" and ruleValue == colori. ruleKey
    * \== "name" and ruleValue == namei.
    *
    * Return the number of items that match the given rule.
    *
    * Example 1:
    *
    * Input: items =
    * [["phone","blue","pixel"],["computer","silver","lenovo"],["phone","gold","iphone"]], ruleKey =
    * "color", ruleValue = "silver" Output: 1 Explanation: There is only one item matching the given
    * rule, which is ["computer","silver","lenovo"].
    *
    * Example 2:
    *
    * Input: items =
    * [["phone","blue","pixel"],["computer","silver","phone"],["phone","gold","iphone"]], ruleKey =
    * "type", ruleValue = "phone" Output: 2 Explanation: There are only two items matching the given
    * rule, which are ["phone","blue","pixel"] and ["phone","gold","iphone"]. Note that the item
    * ["computer","silver","phone"] does not match.
    *
    * Constraints:
    *
    * 1 <= items.length <= 104 1 <= typei.length, colori.length, namei.length, ruleValue.length <=
    * 10 ruleKey is equal to either "type", "color", or "name". All strings consist only of
    * lowercase letters.
    */
  def countMatches(items: List[List[String]], ruleKey: String, ruleValue: String): Int =
    val index = ruleKey match
      case "type"  => 0
      case "color" => 1
      case "name"  => 2
    items.count: item =>
      item(index).equals(ruleValue)

  /** 1832. Check if sentence is pangram.
    *
    * A pangram is a sentence where every letter of the English alphabet appears at least once.
    *
    * Given a string sentence containing only lowercase English letters, return true if sentence is
    * a pangram, or false otherwise.
    *
    * Example 1:
    *
    * Input: sentence = "thequickbrownfoxjumpsoverthelazydog" Output: true Explanation: sentence
    * contains at least one of every letter of the English alphabet.
    *
    * Example 2:
    *
    * Input: sentence = "leetcode" Output: false
    *
    * Constraints:
    *
    * 1 <= sentence.length <= 1000 sentence consists of lowercase English letters.
    */
  def checkIfPangram(sentence: String): Boolean =
    sentence.groupBy(identity).keySet.size == 26

  /** 557. Reverse words in a string III.
    *
    * Given a string s, reverse the order of characters in each word within a sentence while still
    * preserving whitespace and initial word order.
    *
    * Example 1:
    *
    * Input: s = "Let's take LeetCode contest" Output: "s'teL ekat edoCteeL tsetnoc"
    *
    * Example 2:
    *
    * Input: s = "Mr Ding" Output: "rM gniD"
    *
    * Constraints:
    *
    * 1 <= s.length <= 5 * 104 s contains printable ASCII characters. s does not contain any leading
    * or trailing spaces. There is at least one word in s. All the words in s are separated by a
    * single space.
    */
  def reverseWordsIII(s: String): String =
    s.split(" ").map(_.reverse).mkString(" ")

  /** 1859. Sorting the sentence.
    *
    * A sentence is a list of words that are separated by a single space with no leading or trailing
    * spaces. Each word consists of lowercase and uppercase English letters.
    *
    * A sentence can be shuffled by appending the 1-indexed word position to each word then
    * rearranging the words in the sentence.
    *
    * For example, the sentence "This is a sentence" can be shuffled as "sentence4 a3 is2 This1" or
    * "is2 sentence4 This1 a3".
    *
    * Given a shuffled sentence s containing no more than 9 words, reconstruct and return the
    * original sentence.
    *
    * Example 1:
    *
    * Input: s = "is2 sentence4 This1 a3" Output: "This is a sentence" Explanation: Sort the words
    * in s to their original positions "This1 is2 a3 sentence4", then remove the numbers.
    *
    * Example 2:
    *
    * Input: s = "Myself2 Me1 I4 and3" Output: "Me Myself and I" Explanation: Sort the words in s to
    * their original positions "Me1 Myself2 and3 I4", then remove the numbers.
    *
    * Constraints:
    *
    * 2 <= s.length <= 200 s consists of lowercase and uppercase English letters, spaces, and digits
    * from 1 to 9. The number of words in s is between 1 and 9. The words in s are separated by a
    * single space. s contains no leading or trailing spaces.
    */
  def sortSentence(s: String): String =
    s.split(" ")
      .map(s => (s.charAt(s.length - 1).asDigit, s.substring(0, s.length - 1)))
      .sortBy((Int, String) => Int)
      .map:
        case (_, s) => s
      .mkString(" ")

  /** 2810. Faulty keyboard.
    *
    * A sentence is a list of words that are separated by a single space with no leading or trailing
    * spaces. Each word consists of lowercase and uppercase English letters.
    *
    * A sentence can be shuffled by appending the 1-indexed word position to each word then
    * rearranging the words in the sentence.
    *
    * For example, the sentence "This is a sentence" can be shuffled as "sentence4 a3 is2 This1" or
    * "is2 sentence4 This1 a3".
    *
    * Given a shuffled sentence s containing no more than 9 words, reconstruct and return the
    * original sentence.
    *
    * Example 1:
    *
    * Input: s = "is2 sentence4 This1 a3" Output: "This is a sentence" Explanation: Sort the words
    * in s to their original positions "This1 is2 a3 sentence4", then remove the numbers.
    *
    * Example 2:
    *
    * Input: s = "Myself2 Me1 I4 and3" Output: "Me Myself and I" Explanation: Sort the words in s to
    * their original positions "Me1 Myself2 and3 I4", then remove the numbers.
    *
    * Constraints:
    *
    * 2 <= s.length <= 200 s consists of lowercase and uppercase English letters, spaces, and digits
    * from 1 to 9. The number of words in s is between 1 and 9. The words in s are separated by a
    * single space. s contains no leading or trailing spaces.
    */
  def finalString(s: String): String =
    s.foldLeft(""): (acc, e) =>
      e match
        case 'i' => acc.reverse
        case _   => s"$acc$e"

  /** Reverse string II.
    *
    * Given a string s and an integer k, reverse the first k characters for every 2k characters
    * counting from the start of the string.
    *
    * If there are fewer than k characters left, reverse all of them. If there are less than 2k but
    * greater than or equal to k characters, then reverse the first k characters and leave the other
    * as original.
    *
    * Example 1:
    *
    * Input: s = "abcdefg", k = 2 Output: "bacdfeg"
    *
    * Example 2:
    *
    * Input: s = "abcd", k = 2 Output: "bacd"
    *
    * Constraints:
    *
    * 1 <= s.length <= 104 s consists of only lowercase English letters. 1 <= k <= 104
    */
  def reverseStr(s: String, k: Int): String =
    s.grouped(2 * k)
      .map: string2k =>
        if string2k.length() < k then string2k.reverse
        else
          val (x, y) = string2k.splitAt(k)
          x.reverse.concat(y)
      .mkString("")

  /** 2828. Check if a string is an acronym of words.
    *
    * Given an array of strings words and a string s, determine if s is an acronym of words.
    *
    * The string s is considered an acronym of words if it can be formed by concatenating the first
    * character of each string in words in order. For example, "ab" can be formed from ["apple",
    * "banana"], but it can't be formed from ["bear", "aardvark"].
    *
    * Return true if s is an acronym of words, and false otherwise.
    *
    * Example 1:
    *
    * Input: words = ["alice","bob","charlie"], s = "abc" Output: true Explanation: The first
    * character in the words "alice", "bob", and "charlie" are 'a', 'b', and 'c', respectively.
    * Hence, s = "abc" is the acronym.
    *
    * Example 2:
    *
    * Input: words = ["an","apple"], s = "a" Output: false Explanation: The first character in the
    * words "an" and "apple" are 'a' and 'a', respectively. The acronym formed by concatenating
    * these characters is "aa". Hence, s = "a" is not the acronym.
    *
    * Example 3:
    *
    * Input: words = ["never","gonna","give","up","on","you"], s = "ngguoy" Output: true
    * Explanation: By concatenating the first character of the words in the array, we get the string
    * "ngguoy". Hence, s = "ngguoy" is the acronym.
    *
    * Constraints:
    *
    * 1 <= words.length <= 100 1 <= words[i].length <= 10 1 <= s.length <= 100 words[i] and s
    * consist of lowercase English letters.
    */
  def isAcronym(words: List[String], s: String): Boolean =
    words.map(_.charAt(0)).mkString("") == s

  /** 1684. Count the number of consistent strings.
    *
    * You are given a string allowed consisting of distinct characters and an array of strings
    * words. A string is consistent if all characters in the string appear in the string allowed.
    *
    * Return the number of consistent strings in the array words.
    *
    * Example 1:
    *
    * Input: allowed = "ab", words = ["ad","bd","aaab","baa","badab"] Output: 2 Explanation: Strings
    * "aaab" and "baa" are consistent since they only contain characters 'a' and 'b'.
    *
    * Example 2:
    *
    * Input: allowed = "abc", words = ["a","b","c","ab","ac","bc","abc"] Output: 7 Explanation: All
    * strings are consistent.
    *
    * Example 3:
    *
    * Input: allowed = "cad", words = ["cc","acd","b","ba","bac","bad","ac","d"] Output: 4
    * Explanation: Strings "cc", "acd", "ac", and "d" are consistent.
    *
    * Constraints:
    *
    * 1 <= words.length <= 104 1 <= allowed.length <= 26 1 <= words[i].length <= 10 The characters
    * in allowed are distinct. words[i] and allowed contain only lowercase English letters.
    */
  def countConsistentStrings(allowed: String, words: Array[String]): Int =
    val d = allowed.toSet
    words.count:
      _.forall(d.contains(_))

  /** 2418. Sort the people.
    *
    * You are given an array of strings names, and an array heights that consists of distinct
    * positive integers. Both arrays are of length n.
    *
    * For each index i, names[i] and heights[i] denote the name and height of the ith person.
    *
    * Return names sorted in descending order by the people's heights.
    *
    * Example 1:
    *
    * Input: names = ["Mary","John","Emma"], heights = [180,165,170] Output: ["Mary","Emma","John"]
    * Explanation: Mary is the tallest, followed by Emma and John.
    *
    * Example 2:
    *
    * Input: names = ["Alice","Bob","Bob"], heights = [155,185,150] Output: ["Bob","Alice","Bob"]
    * Explanation: The first Bob is the tallest, followed by Alice and the second Bob.
    *
    * Constraints:
    *
    * n == names.length == heights.length 1 <= n <= 103 1 <= names[i].length <= 20 1 <= heights[i]
    * <= 105 names[i] consists of lower and upper case English letters. All the values of heights
    * are distinct.
    */
  def sortPeople(names: Array[String], heights: Array[Int]): Array[String] =
    names
      .zip(heights)
      .sortWith:
        case ((_, h1), (_, h2)) => h1 > h2
      .map(_._1)

  /** 917. Reverse only letters.
    *
    * Given a string s, reverse the string according to the following rules:
    *
    * All the characters that are not English letters remain in the same position. All the English
    * letters (lowercase or uppercase) should be reversed.
    *
    * Return s after reversing it.
    *
    * Example 1:
    *
    * Input: s = "ab-cd" Output: "dc-ba"
    *
    * Example 2:
    *
    * Input: s = "a-bC-dEf-ghIj" Output: "j-Ih-gfE-dCba"
    *
    * Example 3:
    *
    * Input: s = "Test1ng-Leet=code-Q!" Output: "Qedo1ct-eeLg=ntse-T!"
    *
    * Constraints:
    *
    * 1 <= s.length <= 100 s consists of characters with ASCII values in the range [33, 122]. s does
    * not contain '\"' or '\\'.
    */
  def reverseOnlyLetters(s: String): String =
    @scala.annotation.tailrec
    def aux(chars: Array[Char], h: Int, t: Int): String =
      if h < t then
        (chars(h), chars(t)) match
          case (x, y) if x.isLetter && y.isLetter =>
            chars(h) = y
            chars(t) = x
            // println(chars.mkString)
            aux(chars, h + 1, t - 1)
          case (x, y) if x.isLetter && !y.isLetter  => aux(chars, h, t - 1)
          case (x, y) if !x.isLetter && y.isLetter  => aux(chars, h + 1, t)
          case (x, y) if !x.isLetter && !y.isLetter => aux(chars, h + 1, t - 1)
      else chars.mkString("")
    if s.length > 1 then aux(s.toCharArray(), 0, s.length - 1)
    else s

  /** 344. Reverse String.
    *
    * Write a function that reverses a string. The input string is given as an array of characters
    * s.
    *
    * You must do this by modifying the input array in-place with O(1) extra memory.
    *
    * Example 1:
    *
    * Input: s = ["h","e","l","l","o"] Output: ["o","l","l","e","h"]
    *
    * Example 2:
    *
    * Input: s = ["H","a","n","n","a","h"] Output: ["h","a","n","n","a","H"]
    *
    * Constraints:
    *
    * 1 <= s.length <= 105 s[i] is a printable ascii character.
    */
  def reverseString(s: Array[Char]): Unit =
    @scala.annotation.tailrec
    def aux(chars: Array[Char], l: Int, r: Int): Unit =
      if l < r then
        chars(l) = (chars(l) + chars(r)).toChar
        chars(r) = (chars(l) - chars(r)).toChar
        chars(l) = (chars(l) - chars(r)).toChar
        aux(chars, l + 1, r - 1)
    aux(s, 0, s.length - 1)

  /** 1436. Destination city.
    *
    * You are given the array paths, where paths[i] = [cityAi, cityBi] means there exists a direct
    * path going from cityAi to cityBi. Return the destination city, that is, the city without any
    * path outgoing to another city.
    *
    * It is guaranteed that the graph of paths forms a line without any loop, therefore, there will
    * be exactly one destination city.
    *
    * Example 1:
    *
    * - Input: paths = [["London","New York"],["New York","Lima"],["Lima","Sao Paulo"]]
    * - Output: "Sao Paulo"
    * - Explanation: Starting at "London" city you will reach "Sao Paulo" city which is the
    * destination city. Your trip consist of: "London" -> "New York" -> "Lima" -> "Sao Paulo".
    *
    * Example 2:
    *
    * - Input: paths = [["B","C"],["D","B"],["C","A"]]
    * - Output: "A"
    * - Explanation: All possible trips are: "D" -> "B" -> "C" -> "A". "B" -> "C" -> "A". "C" -> "A". "A". Clearly the destination city is "A".
    *
    * Example 3:
    *
    * - Input: paths = [["A","Z"]]
    * - Output: "Z"
    *
    * Constraints:
    *
    * - 1 <= paths.length <= 100
    * - paths[i].length == 2
    * - 1 <= cityAi.length, cityBi.length <= 10 cityAi
    * - != cityBi All strings consist of lowercase and uppercase English letters and the space
    * character.
    *
    * ### Approach
    * - Randomly select a starting point.
    * - Keep travelling to destination until there is no destination
    */
  def destCity(paths: List[List[String]]): String =
    @scala.annotation.tailrec
    def traverse(path: List[String]): String =
      val source = path(0)
      val dest = path(1)
      paths.find(_(0) == dest) match
        case None        => dest
        case Some(nodes) => traverse(nodes)
    traverse(paths(0))

  /** 383. Can construct.
    *
    * Given two strings ransomNote and magazine, return true if ransomNote can be constructed by
    * using the letters from magazine and false otherwise.
    *
    * Each letter in magazine can only be used once in ransomNote.
    *
    * Example 1:
    *
    * Input: ransomNote = "a", magazine = "b" Output: false
    *
    * Example 2:
    *
    * Input: ransomNote = "aa", magazine = "ab" Output: false
    *
    * Example 3:
    *
    * Input: ransomNote = "aa", magazine = "aab" Output: true
    *
    * Constraints:
    *
    * 1 <= ransomNote.length, magazine.length <= 105 ransomNote and magazine consist of lowercase
    * English letters.
    */
  def canConstruct(ransomNote: String, magazine: String): Boolean =
    val mg = magazine
      .toCharArray()
      .groupBy(identity)
      .map: (e: Char, chars: Array[Char]) =>
        (e -> chars.size)
    val rg = ransomNote
      .toCharArray()
      .groupBy(identity)
      .map: (e: Char, chars: Array[Char]) =>
        (e -> chars.size)
    rg.forall: (c: Char, f: Int) =>
      mg.contains(c) && f <= mg(c)

  /** 2047. Number of valid words in a sentence.
    *
    * A sentence consists of lowercase letters ('a' to 'z'), digits ('0' to '9'), hyphens ('-'),
    * punctuation marks ('!', '.', and ','), and spaces (' ') only. Each sentence can be broken down
    * into one or more tokens separated by one or more spaces ' '.
    *
    * A token is a valid word if all three of the following are true:
    *
    * It only contains lowercase letters, hyphens, and/or punctuation (no digits). There is at most
    * one hyphen '-'. If present, it must be surrounded by lowercase characters ("a-b" is valid, but
    * "-ab" and "ab-" are not valid). There is at most one punctuation mark. If present, it must be
    * at the end of the token ("ab,", "cd!", and "." are valid, but "a!b" and "c.," are not valid).
    *
    * Examples of valid words include "a-b.", "afad", "ba-c", "a!", and "!".
    *
    * Given a string sentence, return the number of valid words in sentence.
    *
    * Example 1:
    *
    *   - Input: sentence = "cat and dog"
    *   - Output: 3
    *   - Explanation: The valid words in the sentence are "cat", "and", and "dog".
    *
    * Example 2:
    *
    *   - Input: sentence = "!this 1-s b8d!"
    *   - Output: 0
    *   - Explanation: There are no valid words in the sentence. "!this" is invalid because it
    *     starts with a punctuation mark. "1-s" and "b8d" are invalid because they contain digits.
    *
    * Example 3:
    *
    *   - Input: sentence = "alice and bob are playing stone-game10"
    *   - Output: 5
    *   - Explanation: The valid words in the sentence are "alice", "and", "bob", "are", and
    *     "playing". "stone-game10" is invalid because it contains digits.
    *
    * Constraints:
    *
    *   - 1 <= sentence.length <= 1000
    *   - sentence only contains lowercase English letters, digits, ' ', * '-', '!', '.', and ','.
    *     There will be at least 1 token.
    */
  def countValidWords(sentence: String): Int =

    lazy val punctationSet = Set('!', ',', '.')
    lazy val hyphen = '-'

    val ruleForCharset: String => Boolean = s =>
      s.forall: c =>
        c.isLower || c == hyphen || punctationSet.contains(c)

    val ruleForHyphen: String => Boolean = s =>
      s.count(_ == hyphen) == 0 ||
        (s.count(_ == hyphen) == 1 && s.indexOf(hyphen) > 0 && s.indexOf(hyphen) <= s.length - 2 &&
          (
            (s(s.indexOf(hyphen) - 1), s(s.indexOf(hyphen) + 1)) match
              case (x, y) => ('a' to 'z').contains(x) && ('a' to 'z').contains(y)
          ))

    val ruleForPunctuation: String => Boolean = s =>
      s.count(punctationSet.contains(_)) == 0 ||
        (s.count(punctationSet.contains(_)) == 1 &&
          s.indexWhere(punctationSet.contains(_)) == s.length - 1)

    val res =
      sentence
        .split(" ")
        .map(_.trim)
        .filter(!_.isEmpty())
        .filter(ruleForCharset)
        .filter(ruleForHyphen)
        .filter(ruleForPunctuation)
    res.length

  /** 2138. Divide a string into group of size k.
    *
    * A string s can be partitioned into groups of size k using the following procedure:
    *
    * The first group consists of the first k characters of the string, the second group consists of the next k characters of the string, and so on. Each character can be a part of exactly one
    * group. For the last group, if the string does not have k characters remaining, a character
    * fill is used to complete the group.
    *
    * Note that the partition is done so that after removing the fill character from the last group (if it exists) and concatenating all the groups in order, the resultant string should be s.
    *
    * Given the string s, the size of each group k and the character fill, return a string array
    * denoting the composition of every group s has been divided into, using the above procedure.
    *
    * Example 1:
    *
    * Input: s = "abcdefghi", k = 3, fill = "x" Output: ["abc","def","ghi"] Explanation: The first 3
    * characters "abc" form the first group. The next 3 characters "def" form the second group. The
    * last 3 characters "ghi" form the third group. Since all groups can be completely filled by
    * characters from the string, we do not need to use fill. Thus, the groups formed are "abc",
    * "def", and "ghi".
    *
    * Example 2:
    *
    * Input: s = "abcdefghij", k = 3, fill = "x" Output: ["abc","def","ghi","jxx"] Explanation:
    * Similar to the previous example, we are forming the first three groups "abc", "def", and
    * "ghi". For the last group, we can only use the character 'j' from the string. To complete this
    * group, we add 'x' twice. Thus, the 4 groups formed are "abc", "def", "ghi", and "jxx".
    *
    * Constraints:
    *
    * 1 <= s.length <= 100 s consists of lowercase English letters only. 1 <= k <= 100 fill is a
    * lowercase English letter.
    */
  def divideString(s: String, k: Int, fill: Char): Array[String] =
    val required = if s.length % k != 0 then k - s.length % k else 0
    s.concat(Array.fill(required)(fill).mkString).grouped(k).toArray

  /** 2108. Find first palindromic string in the array.
    *
    * Given an array of strings words, return the first palindromic string in the array. If there is no such string, return an empty string "".
    *
    * A string is palindromic if it reads the same forward and backward.
    *
    * Example 1:
    *
    * Input: words = ["abc","car","ada","racecar","cool"] Output: "ada" Explanation: The first
    * string that is palindromic is "ada". Note that "racecar" is also palindromic, but it is not
    * the first.
    *
    * Example 2:
    *
    * Input: words = ["notapalindrome","racecar"] Output: "racecar" Explanation: The first and only
    * string that is palindromic is "racecar".
    *
    * Example 3:
    *
    * Input: words = ["def","ghi"] Output: "" Explanation: There are no palindromic strings, so the
    * empty string is returned.
    *
    * Constraints:
    *
    * 1 <= words.length <= 100 1 <= words[i].length <= 100 words[i] consists only of lowercase
    * English letters.
    */
  def firstPalindrome(words: Array[String]): String =
    def isPalindrome(s: String): Boolean =
      @scala.annotation.tailrec
      def helper(head: Int, tail: Int): Boolean = (head, tail) match
        case (h, t) if h <= t => s(h) == s(t) && helper(h + 1, t - 1)
        case _                => true
      helper(head = 0, tail = s.length - 1)

    words.find(isPalindrome) match
      case Some(s) => s
      case None    => ""

  /** 125. Valid palindrome.
    *
    * A phrase is a palindrome if, after converting all uppercase letters into lowercase letters and
    * removing all non-alphanumeric characters, it reads the same forward and backward. Alphanumeric
    * characters include letters and numbers.
    *
    * Given a string s, return true if it is a palindrome, or false otherwise.
    *
    * Example 1:
    *
    * Input: s = "A man, a plan, a canal: Panama" Output: true Explanation: "amanaplanacanalpanama"
    * is a palindrome.
    *
    * Example 2:
    *
    * Input: s = "race a car" Output: false Explanation: "raceacar" is not a palindrome.
    *
    * Example 3:
    *
    * Input: s = " " Output: true Explanation: s is an empty string "" after removing
    * non-alphanumeric characters. Since an empty string reads the same forward and backward, it is
    * a palindrome.
    *
    * Constraints:
    *
    * 1 <= s.length <= 2 * 10^5 s consists only of printable ASCII characters.
    */
  def isPalindrome(s: String): Boolean =
    @scala.annotation.tailrec
    def aux(head: Int = 0, tail: Int): Boolean =
      // println(s"$s => ${s(head)} vs ${s(tail)}")
      if head <= tail then
        (s(head), s(tail)) match
          case (x, y) if x.isLetterOrDigit && y.isLetterOrDigit =>
            x.toLower == y.toLower && aux(head + 1, tail - 1)
          case (x, y) if x.isLetterOrDigit && !y.isLetterOrDigit  => aux(head, tail - 1)
          case (x, y) if !x.isLetterOrDigit && y.isLetterOrDigit  => aux(head + 1, tail)
          case (x, y) if !x.isLetterOrDigit && !y.isLetterOrDigit => aux(head + 1, tail - 1)
      else true
    s.trim.isEmpty || aux(head = 0, tail = s.length - 1)

  /** 434. Number of segments in a string.
    *
    * Given a string s, return the number of segments in the string.
    *
    * A segment is defined to be a contiguous sequence of non-space characters.
    *
    * Example 1:
    *
    * Input: s = "Hello, my name is John" Output: 5 Explanation: The five segments are ["Hello,",
    * "my", "name", "is", "John"]
    *
    * Example 2:
    *
    * Input: s = "Hello" Output: 1
    *
    * Constraints:
    *
    * 0 <= s.length <= 300 s consists of lowercase and uppercase English letters, digits, or one of
    * the following characters "!@#$%^&*()_+-=',.:". The only space character in s is ' '.
    */
  def countSegments(s: String): Int =
    s.split(' ').filter(!_.isEmpty()).size

  /** 1768. Merge strings alternatively.
    *
    * You are given two strings word1 and word2. Merge the strings by adding letters in alternating
    * order, starting with word1. If a string is longer than the other, append the additional
    * letters onto the end of the merged string.
    *
    * Return the merged string.
    *
    * Example 1:
    *
    * - Input: word1 = "abc", word2 = "pqr"
    * - Output: "apbqcr"
    * - Explanation: The merged string will be merged as so: word1: a b c word2: p q r merged: a p b q c r
    *
    * Example 2:
    *
    * - Input: word1 = "ab", word2 = "pqrs"
    * - Output: "apbqrs"
    * - Explanation: Notice that as word2 is longer, "rs" is appended to the end. word1: a b word2: p q r s merged: a p b q r s
    *
    * Example 3:
    *
    * - Input: word1 = "abcd", word2 = "pq"
    * - Output: "apbqcd"
    * - Explanation: Notice that as word1 is longer, "cd" is appended to the end. word1: a b c d word2: p q merged: a p b q c d
    *
    * Constraints:
    *
    *   - 1 <= word1.length, word2.length <= 100
    *   - word1 and word2 consist of lowercase English letters.
    *
    * ### Approach
    *   - zipAll with place holders is easier to maintain.
    *   - another approach is to loop and maintain the indices.
    *
    * @note
    *   programming-skills
    */
  def mergeAlternately(word1: String, word2: String): String =
    word1
      .zipAll(word2, "", "")
      .map: (a, b) =>
        s"$a$b"
      .mkString

  /** 1614. Maximum nesting depth of the parantheses.
    *
    * A string is a valid parentheses string (denoted VPS) if it meets one of the following:
    *
    * It is an empty string "", or a single character not equal to "(" or ")", It can be written as
    * AB (A concatenated with B), where A and B are VPS's, or It can be written as (A), where A is a
    * VPS.
    *
    * We can similarly define the nesting depth depth(S) of any VPS S as follows:
    *
    * depth("") = 0 depth(C) = 0, where C is a string with a single character not equal to "(" or
    * ")". depth(A + B) = max(depth(A), depth(B)), where A and B are VPS's. depth("(" + A + ")") = 1
    * + depth(A), where A is a VPS.
    *
    * For example, "", "()()", and "()(()())" are VPS's (with nesting depths 0, 1, and 2), and ")("
    * and "(()" are not VPS's.
    *
    * Given a VPS represented as string s, return the nesting depth of s.
    *
    * Example 1:
    *
    * Input: s = "(1+(2*3)+((8)/4))+1" Output: 3 Explanation: Digit 8 is inside of 3 nested
    * parentheses in the string.
    *
    * Example 2:
    *
    * Input: s = "(1)+((2))+(((3)))" Output: 3
    *
    * Constraints:
    *
    * 1 <= s.length <= 100 s consists of digits 0-9 and characters '+', '-', '*', '/', '(', and ')'.
    * It is guaranteed that parentheses expression s is a VPS.
    */
  def maxDepth(s: String): Int =
    @scala.annotation.tailrec
    def aux(index: Int, currentDepth: Int, currentMax: Int): Int =
      if index <= s.length - 1 then
        s(index) match
          case c if c == '(' => aux(index + 1, currentDepth + 1, currentMax)
          case c if c == ')' => aux(index + 1, currentDepth - 1, currentDepth max currentMax)
          case _             => aux(index + 1, currentDepth, currentMax)
      else currentDepth max currentMax
    aux(index = 0, currentDepth = 0, currentMax = 0)

  /** 2710. Removing trailing zeroes from a string.
    *
    * Given a positive integer num represented as a string, return the integer num without trailing
    * zeros as a string.
    *
    * Example 1:
    *
    * Input: num = "51230100" Output: "512301" Explanation: Integer "51230100" has 2 trailing zeros,
    * we remove them and return integer "512301".
    *
    * Example 2:
    *
    * Input: num = "123" Output: "123" Explanation: Integer "123" has no trailing zeros, we return
    * integer "123".
    *
    * Constraints:
    *
    * 1 <= num.length <= 1000 num consists of only digits. num doesn't have any leading zeros.
    */
  def removeTrailingZeros(num: String): String =
    // num.dropRight(num.reverse.takeWhile(_ == '0').length)
    num.reverse.dropWhile(_ == '0').reverse

  /** 2678. Number of senior citizens.
    *
    * You are given a 0-indexed array of strings details. Each element of details provides
    * information about a given passenger compressed into a string of length 15. The system is such
    * that:
    *
    * The first ten characters consist of the phone number of passengers. The next character denotes
    * the gender of the person. The following two characters are used to indicate the age of the
    * person. The last two characters determine the seat allotted to that person.
    *
    * Return the number of passengers who are strictly more than 60 years old.
    *
    * Example 1:
    *
    * Input: details = ["7868190130M7522","5303914400F9211","9273338290F4010"] Output: 2
    * Explanation: The passengers at indices 0, 1, and 2 have ages 75, 92, and 40. Thus, there are 2
    * people who are over 60 years old.
    *
    * Example 2:
    *
    * Input: details = ["1313579440F2036","2921522980M5644"] Output: 0 Explanation: None of the
    * passengers are older than 60.
    *
    * Constraints:
    *
    * 1 <= details.length <= 100 details[i].length == 15 details[i] consists of digits from '0' to
    * '9'. details[i][10] is either 'M' or 'F' or 'O'. The phone numbers and seat numbers of the
    * passengers are distinct.
    */
  def countSeniors(details: Array[String]): Int =
    details
      .map(_.substring(11, 13))
      .map(_.toInt)
      .count(_ > 60)

  /** 1544. Make the string great.
    *
    * Given a string s of lower and upper case English letters.
    *
    * A good string is a string which doesn't have two adjacent characters s[i] and s[i + 1] where:
    *
    * 0 <= i <= s.length - 2 s[i] is a lower-case letter and s[i + 1] is the same letter but in
    * upper-case or vice-versa.
    *
    * To make the string good, you can choose two adjacent characters that make the string bad and
    * remove them. You can keep doing this until the string becomes good.
    *
    * Return the string after making it good. The answer is guaranteed to be unique under the given
    * constraints.
    *
    * Notice that an empty string is also good.
    *
    * Example 1:
    *
    * Input: s = "leEeetcode" Output: "leetcode" Explanation: In the first step, either you choose i
    * \= 1 or i = 2, both will result "leEeetcode" to be reduced to "leetcode".
    *
    * Example 2:
    *
    * Input: s = "abBAcC" Output: "" Explanation: We have many possible scenarios, and all lead to
    * the same answer. For example: "abBAcC" --> "aAcC" --> "cC" --> "" "abBAcC" --> "abBA" --> "aA"
    * --> ""
    *
    * Example 3:
    *
    * Input: s = "s" Output: "s"
    *
    * Constraints:
    *
    * 1 <= s.length <= 100 s contains only lower and upper case English letters.
    *
    * ### Approach
    *
    * - using stack is a much optimized and readable solution yet this is done for the heck of it.
    * - implemented as I was struggling to implement for 2 hours after solving quickly on paper.
    */
  def makeGood(s: String): String =
    @scala.annotation.tailrec
    def loop(string: String, i: Int): String =
      if i + 1 < string.length then
        (string(i), string(i + 1)) match
          case (x, y) if math.abs(x - y) == 32 =>
            // println(s"found dirty pair '$x$y' @ '$i'.")
            val newstring = string.substring(0, i).concat(string.substring(i + 2))
            // println(s"$string => $newstring")
            loop(newstring, 0)
          case (x, y) =>
            // println(s"\t'$x vs $y is ok'")
            loop(string, i + 1)
      else string
    loop(s, 0)

  /** 58. Length of the last word.
    *
    * Given a string s consisting of words and spaces, return the length of the last word in the
    * string.
    *
    * A word is a maximal substring consisting of non-space characters only.
    *
    * Example 1:
    *
    *   - Input: s = "Hello World"
    *   - Output: 5
    *   - Explanation: The last word is "World" with length 5.
    *
    * Example 2:
    *
    *   - Input: s = " fly me to the moon "
    *   - Output: 4
    *   - Explanation: The last word is "moon" with length 4.
    *
    * Example 3:
    *
    *   - Input: s = "luffy is still joyboy"
    *   - Output: 6
    *   - Explanation: The last word is "joyboy" with length 6.
    *
    * Constraints:
    *
    *   - 1 <= s.length <= 10^4
    *   - s consists of only English letters and spaces ' '.
    *   - There will be at least one word in s.
    *
    * @note
    *   programming-skills
    */
  def lengthOfLastWord(s: String): Int =
    // "\\s*[\\w]*\\s*$".r
    //   .findFirstIn(s) match
    //   case None => 0
    //   case Some(value) =>
    //     value.trim.length()
    s.split(" ").last.length

  /** 28. Find the index of first occurence in a string.
    *
    * Given two strings needle and haystack, return the index of the first occurrence of needle in
    * haystack, or -1 if needle is not part of haystack.
    *
    * Example 1:
    *
    * Input: haystack = "sadbutsad", needle = "sad" Output: 0 Explanation: "sad" occurs at index 0
    * and 6. The first occurrence is at index 0, so we return 0.
    *
    * Example 2:
    *
    * Input: haystack = "leetcode", needle = "leeto" Output: -1 Explanation: "leeto" did not occur
    * in "leetcode", so we return -1.
    *
    * Constraints:
    * - 1 <= haystack.length, needle.length <= 104
    * - haystack and needle consist of only lowercase english characters.
    *
    * @note
    *   programming-skills
    */
  def strStr(haystack: String, needle: String): Int =
    haystack.indexOf(needle)

  /** 20. Valid parantheses.
    *
    * Given a string s containing just the characters '(', ')', '{', '}', '[' and ']', determine if the input string is valid.
    *
    * An input string is valid if:
    *
    * Open brackets must be closed by the same type of brackets. Open brackets must be closed in the correct order. Every close bracket has a corresponding open bracket of the same type.
    *
    * Example 1:
    *
    * Input: s = "()" Output: true
    *
    * Example 2:
    *
    * Input: s = "()[]{}" Output: true
    *
    * Example 3:
    *
    * Input: s = "(]" Output: false
    *
    * Constraints:
    *
    * 1 <= s.length <= 104 s consists of parentheses only '()[]{}'.
    */
  def isValid(s: String): Boolean =
    @scala.annotation.tailrec
    def aux(string: String, acc: List[Char]): Boolean =
      string.headOption match
        case None => acc.size == 0
        case Some(char) =>
          char match
            case ')' | '}' | ']' if acc.isEmpty => false
            case c @ ('(' | '{' | '[')          => aux(string.tail, c :: acc)
            case ')' if acc.head == '('         => aux(string.tail, acc.tail)
            case '}' if acc.head == '{'         => aux(string.tail, acc.tail)
            case ']' if acc.head == '['         => aux(string.tail, acc.tail)
            case _                              => false
    aux(s, List.empty[Char])

  /** 1249. Minimum remove to make valid paranthesis.
    *
    * Given a string s of '(' , ')' and lowercase English characters.
    *
    * Your task is to remove the minimum number of parentheses ( '(' or ')', in any positions ) so
    * that the resulting parentheses string is valid and return any valid string.
    *
    * Formally, a parentheses string is valid if and only if:
    *
    * It is the empty string, contains only lowercase characters, or It can be written as AB (A
    * concatenated with B), where A and B are valid strings, or It can be written as (A), where A is
    * a valid string.
    *
    * Example 1:
    *
    * Input: s = "lee(t(c)o)de)" Output: "lee(t(c)o)de" Explanation: "lee(t(co)de)" , "lee(t(c)ode)"
    * would also be accepted.
    *
    * Example 2:
    *
    * Input: s = "a)b(c)d" Output: "ab(c)d"
    *
    * Example 3:
    *
    * Input: s = "))((" Output: "" Explanation: An empty string is also valid.
    *
    * Constraints:
    *
    * 1 <= s.length <= 105 s[i] is either'(' , ')', or lowercase English letter.
    *
    * ### Approach
    *
    * - scan left to right and ignore excess right parantheses
    * - scan right to left and ignore excess left parantheses
    */
  def minRemoveToMakeValid(s: String): String =
    val (leftFixedReversedList, _) =
      s.foldLeft(List.empty[Char], 0): (t, char) =>
        (t, char) match
          case ((acc, l), '(') => ('(' :: acc, l + 1)
          case ((acc, l), ')') => if l > 0 then (')' :: acc, l - 1) else (acc, l) // ')' ignored
          case ((acc, l), c)   => (c :: acc, l)

    val (rFixedList, _) =
      leftFixedReversedList.foldLeft(List.empty[Char], 0): (t, char) =>
        (t, char) match
          case ((acc, r), '(') => if r > 0 then ('(' :: acc, r - 1) else (acc, r) // '(' ignored
          case ((acc, r), ')') => (')' :: acc, r + 1)
          case ((acc, r), c)   => (c :: acc, r)

    // println(s"first: $s => ${leftFixedReversedList.mkString.reverse} => ${rFixedList.mkString}")
    rFixedList.mkString

  /** 1678. Goal parser interpretation.
    *
    * You own a Goal Parser that can interpret a string command. The command consists of an alphabet
    * of "G", "()" and/or "(al)" in some order. The Goal Parser will interpret "G" as the string
    * "G", "()" as the string "o", and "(al)" as the string "al". The interpreted strings are then
    * concatenated in the original order.
    *
    * Given the string command, return the Goal Parser's interpretation of command.
    *
    * Example 1:
    *
    * Input: command = "G()(al)" Output: "Goal" Explanation: The Goal Parser interprets the command
    * as follows: G -> G () -> o (al) -> al The final concatenated result is "Goal".
    *
    * Example 2:
    *
    * Input: command = "G()()()()(al)" Output: "Gooooal"
    *
    * Example 3:
    *
    * Input: command = "(al)G(al)()()G" Output: "alGalooG"
    *
    * Constraints:
    *
    * 1 <= command.length <= 100 command consists of "G", "()", and/or "(al)" in some order.
    */
  def interpret(command: String): String =
    // scan left to right, collect chars into token until delimiters 'G' or ')' are met.
    // when delimiters are met, evaluate token and append to 'acc'
    val (acc, _) =
      command.foldLeft("", ""): (state, char) =>
        (state, char) match
          case ((acc, _), e @ 'G')                       => (acc.concat("G"), "")
          case ((acc, token), e @ ')') if token == "("   => (acc.concat("o"), "")
          case ((acc, token), e @ ')') if token == "(al" => (acc.concat("al"), "")
          case ((acc, token), e)                         => (acc, s"$token$e")
    acc

  /** 678. Valid paranthesis string.
    *
    * Given a string s containing only three types of characters: '(', ')' and '*', return true if s
    * is valid.
    *
    * The following rules define a valid string:
    *
    * Any left parenthesis '(' must have a corresponding right parenthesis ')'. Any right
    * parenthesis ')' must have a corresponding left parenthesis '('. Left parenthesis '(' must go
    * before the corresponding right parenthesis ')'. '*' could be treated as a single right
    * parenthesis ')' or a single left parenthesis '(' or an empty string "".
    *
    * Example 1:
    *
    * Input: s = "()" Output: true
    *
    * Example 2:
    *
    * Input: s = "(*)" Output: true
    *
    * Example 3:
    *
    * Input: s = "(*))" Output: true
    *
    * Constraints:
    *
    * 1 <= s.length <= 100 s[i] is '(', ')' or '*'.
    */
  def checkValidString(s: String): Boolean =
    def validator(string: String, pc: Int, jc: Int): Boolean =
      string.headOption match
        case None =>
          // println(s"pc: $pc, joker: $jc")
          jc >= math.abs(pc)
        case Some('(')                       => validator(string.tail, pc + 1, jc)
        case Some('*')                       => validator(string.tail, pc, jc + 1)
        case Some(')') if pc == 0 && jc == 0 => false
        case Some(')')                       => validator(string.tail, pc - 1, jc)
        case _                               => validator(string.tail, pc, jc)
    validator(s, 0, 0)

  /** 389. Find the difference.
    *
    * You are given two strings s and t.
    *
    * String t is generated by random shuffling string s and then add one more letter at a random
    * position.
    *
    * Return the letter that was added to t.
    *
    * Example 1:
    *
    * Input: s = "abcd", t = "abcde" Output: "e" Explanation: 'e' is the letter that was added.
    *
    * Example 2:
    *
    * Input: s = "", t = "y" Output: "y"
    *
    * Constraints:
    *
    *   - 0 <= s.length <= 1000
    *   - t.length == s.length + 1 s
    *   - t consist of lowercase English letters.
    *
    * ### Approach
    *   - Sum all chars of s & t.
    *   - Find the difference and return the character equivalent of the difference.
    *
    * @note
    *   programming-skills
    */
  def findTheDifference(s: String, t: String): Char =
    (t.sum - s.sum).toChar

  /** 2000. Reverse prefix of a word.
    *
    * Given a 0-indexed string word and a character ch, reverse the segment of word that starts at
    * index 0 and ends at the index of the first occurrence of ch (inclusive). If the character ch
    * does not exist in word, do nothing.
    *
    * For example, if word = "abcdefd" and ch = "d", then you should reverse the segment that starts at 0 and ends at 3 (inclusive). The resulting string will be "dcbaefd".
    *
    * Return the resulting string.
    *
    * Example 1:
    *
    * Input: word = "abcdefd", ch = "d" Output: "dcbaefd" Explanation: The first occurrence of "d"
    * is at index 3. Reverse the part of word from 0 to 3 (inclusive), the resulting string is
    * "dcbaefd".
    *
    * Example 2:
    *
    * Input: word = "xyxzxe", ch = "z" Output: "zxyxxe" Explanation: The first and only occurrence
    * of "z" is at index 3. Reverse the part of word from 0 to 3 (inclusive), the resulting string
    * is "zxyxxe".
    *
    * Example 3:
    *
    * Input: word = "abcd", ch = "z" Output: "abcd" Explanation: "z" does not exist in word. You
    * should not do any reverse operation, the resulting string is "abcd".
    *
    * Constraints:
    *
    * 1 <= word.length <= 250 word consists of lowercase English letters. ch is a lowercase English
    * letter.
    */
  def reversePrefix(word: String, ch: Char): String =
    if word.contains(ch) then
      val index = word.indexOf(ch)
      word.substring(0, index + 1).reverse.concat(word.substring(index + 1))
    else word

  /** 3019. Number of changing keys.
    *
    * You are given a 0-indexed string s typed by a user. Changing a key is defined as using a key
    * different from the last used key. For example, s = "ab" has a change of a key while s = "bBBb" does not have any.
    *
    * Return the number of times the user had to change the key.
    *
    * Note: Modifiers like shift or caps lock won't be counted in changing the key that is if a user typed the letter 'a' and then the letter 'A' then it will not be considered as a changing of key.
    *
    * Example 1:
    *
    * Input: s = "aAbBcC" Output: 2 Explanation: From s[0] = 'a' to s[1] = 'A', there is no change
    * of key as caps lock or shift is not counted. From s[1] = 'A' to s[2] = 'b', there is a change of key. From s[2] = 'b' to s[3] = 'B', there is no change of key as caps lock or shift is not counted. From s[3] = 'B' to s[4] = 'c', there is a change of key. From s[4] = 'c' to s[5] = 'C', there is no change of key as caps lock or shift is not counted.
    *
    * Example 2:
    *
    * Input: s = "AaAaAaaA" Output: 0 Explanation: There is no change of key since only the letters
    * 'a' and 'A' are pressed which does not require change of key.
    *
    * Constraints:
    *
    * 1 <= s.length <= 100 s consists of only upper case and lower case English letters.
    */
  def countKeyChanges(s: String): Int =
    // if s.length == 1 then 0
    // else
    //   s.toLowerCase()
    //     .sliding(2)
    //     .toList
    //     .foldLeft(0): (acc, charPair) =>
    //       if charPair(0).equals(charPair(1)) then acc
    //       else acc + 1
    (1 until s.length).count(i => s(i - 1).toLower != s(i).toLower)

  /** 1221. Split a string balanced strings.
    *
    * Balanced strings are those that have an equal quantity of 'L' and 'R' characters.
    *
    * Given a balanced string s, split it into some number of substrings such that:
    *
    * Each substring is balanced.
    *
    * Return the maximum number of balanced strings you can obtain.
    *
    * Example 1:
    *
    * Input: s = "RLRRLLRLRL" Output: 4 Explanation: s can be split into "RL", "RRLL", "RL", "RL",
    * each substring contains same number of 'L' and 'R'.
    *
    * Example 2:
    *
    * Input: s = "RLRRRLLRLL" Output: 2 Explanation: s can be split into "RL", "RRRLLRLL", each
    * substring contains same number of 'L' and 'R'. Note that s cannot be split into "RL", "RR",
    * "RL", "LR", "LL", because the 2nd and 5th substrings are not balanced.
    *
    * Example 3:
    *
    * Input: s = "LLLLRRRR" Output: 1 Explanation: s can be split into "LLLLRRRR".
    *
    * Constraints:
    *
    * 2 <= s.length <= 1000 s[i] is either 'L' or 'R'. s is a balanced string.
    */
  def balancedStringSplit(s: String): Int =
    val (_, counter) = s.foldLeft(0, 0): (state, e) =>
      (state, e) match
        case ((cur, acc), 'R') =>
          if cur + 1 == 0 then (0, acc + 1)
          else (cur + 1, acc)
        case ((cur, acc), 'L') =>
          if cur - 1 == 0 then (0, acc + 1)
          else (cur - 1, acc)
        case (t, e) => t
    counter

  /** 1528. Shuffle the string.
    *
    * You are given a string s and an integer array indices of the same length. The string s will be
    * shuffled such that the character at the ith position moves to indices[i] in the shuffled
    * string.
    *
    * Return the shuffled string.
    *
    * Example 1:
    *
    * Input: s = "codeleet", indices = [4,5,6,7,0,2,1,3] Output: "leetcode" Explanation: As shown,
    * "codeleet" becomes "leetcode" after shuffling.
    *
    * Example 2:
    *
    * Input: s = "abc", indices = [0,1,2] Output: "abc" Explanation: After shuffling, each character
    * remains in its position.
    *
    * Constraints:
    *
    * s.length == indices.length == n 1 <= n <= 100 s consists of only lowercase English letters. 0
    * <= indices[i] < n All values of indices are unique.
    */
  def restoreString(s: String, indices: Array[Int]): String =
    s.toCharArray()
      .zip(indices)
      .sortBy(_._2)
      .map(_._1)
      .mkString

  /** 2325. Decode the message.
    *
    * You are given the strings key and message, which represent a cipher key and a secret message,
    * respectively. The steps to decode message are as follows:
    *
    * Use the first appearance of all 26 lowercase English letters in key as the order of the
    * substitution table. Align the substitution table with the regular English alphabet. Each
    * letter in message is then substituted using the table. Spaces ' ' are transformed to
    * themselves.
    *
    * For example, given key = "happy boy" (actual key would have at least one instance of each
    * letter in the alphabet), we have the partial substitution table of ('h' -> 'a', 'a' -> 'b',
    * 'p' -> 'c', 'y' -> 'd', 'b' -> 'e', 'o' -> 'f').
    *
    * Return the decoded message.
    *
    * Example: 1
    *
    * Input: key = "the quick brown fox jumps over the lazy dog", message = "vkbs bs t suepuv"
    * Output: "this is a secret" Explanation: The diagram above shows the substitution table. It is
    * obtained by taking the first appearance of each letter in "the quick brown fox jumps over the
    * lazy dog".
    *
    * Example 2: Input: key = "eljuxhpwnyrdgtqkviszcfmabo", message = "zwx hnfx lqantp mnoeius ycgk
    * vcnjrdb" Output: "the five boxing wizards jump quickly" Explanation: The diagram above shows
    * the substitution table. It is obtained by taking the first appearance of each letter in
    * "eljuxhpwnyrdgtqkviszcfmabo".
    *
    * Constraints:
    *
    * 26 <= key.length <= 2000 key consists of lowercase English letters and ' '. key contains every
    * letter in the English alphabet ('a' to 'z') at least once. 1 <= message.length <= 2000 message
    * consists of lowercase English letters and ' '.
    */
  def decodeMessage(key: String, message: String): String =
    val substitutionTable = key.distinct.filter(_ != ' ')
    // println(s"substitution table: $substitutionTable")
    message
      .toCharArray()
      .map: c =>
        if c == ' ' then c else (substitutionTable.indexOf(c) + 'a').toChar
      .mkString

  /** 67. Add binary.
    *
    * Given two binary strings a and b, return their sum as a binary string.
    *
    * Example 1:
    *
    * Input: a = "11", b = "1" Output: "100"
    *
    * Example 2:
    *
    * Input: a = "1010", b = "1011" Output: "10101"
    *
    * Constraints:
    *
    * 1 <= a.length, b.length <= 104 a and b consist only of '0' or '1' characters. Each string does
    * not contain leading zeros except for the zero itself.
    */
  def addBinary(a: String, b: String): String =
    @scala.annotation.tailrec
    def loop(ar: String, br: String, carryOver: Int = 0, acc: String = ""): String =
      (ar, br) match
        case ("", "") =>
          if acc.isEmpty() then s"$carryOver"
          else
            carryOver match
              case 1 => s"$carryOver$acc"
              case _ => acc
        case (x, y) =>
          val ac = if x == "" then 0 else x.head - '0'
          val bc = if y == "" then 0 else y.head - '0'
          val sum = ac + bc + carryOver
          loop(ar.tail, br.tail, sum / 2, s"${sum % 2}$acc")
    loop(a.reverse, b.reverse)

  /** 415. Add strings.
    *
    * Given two non-negative integers, num1 and num2 represented as string, return the sum of num1
    * and num2 as a string.
    *
    * You must solve the problem without using any built-in library for handling large integers
    * (such as BigInteger). You must also not convert the inputs to integers directly.
    *
    * Example 1:
    *
    * Input: num1 = "11", num2 = "123" Output: "134"
    *
    * Example 2:
    *
    * Input: num1 = "456", num2 = "77" Output: "533"
    *
    * Example 3:
    *
    * Input: num1 = "0", num2 = "0" Output: "0"
    *
    * Constraints:
    *
    * 1 <= num1.length, num2.length <= 104 num1 and num2 consist of only digits. num1 and num2 don't
    * have any leading zeros except for the zero itself.
    */
  def addStrings(num1: String, num2: String): String =
    @scala.annotation.tailrec
    def loop(a: String, b: String, carryOver: Int = 0, acc: String = ""): String =
      (a, b) match
        case ("", "") =>
          (carryOver, acc) match
            case (c, acc) if acc.isEmpty() => c.toString
            case (0, acc)                  => acc
            case (c, acc)                  => s"$c$acc"
        case (x, y) =>
          val xi = if x == "" then 0 else x.head.asDigit
          val yi = if y == "" then 0 else y.head.asDigit
          val sum = xi + yi + carryOver
          loop(a.tail, b.tail, sum / 10, s"${sum % 10}$acc")
    loop(num1.reverse, num2.reverse)

  /** 2864. Maximum odd binary number. You are given a binary string s that contains at least one
    * '1'.
    *
    * You have to rearrange the bits in such a way that the resulting binary number is the maximum
    * odd binary number that can be created from this combination.
    *
    * Return a string representing the maximum odd binary number that can be created from the given
    * combination.
    *
    * Note that the resulting string can have leading zeros.
    *
    * Example 1:
    *
    * Input: s = "010" Output: "001" Explanation: Because there is just one '1', it must be in the
    * last position. So the answer is "001".
    *
    * Example 2:
    *
    * Input: s = "0101" Output: "1001" Explanation: One of the '1's must be in the last position.
    * The maximum number that can be made with the remaining digits is "100". So the answer is
    * "1001".
    */
  def maximumOddBinaryNumber(s: String): String =
    val zeros = s.count(_ == '0')
    val ones = s.count(_ == '1')
    ("1" * (ones - 1)) + ("0" * zeros) + "1"

  /** 804. Unique morse code words
    *
    * International Morse Code defines a standard encoding where each letter is mapped to a series
    * of dots and dashes, as follows:
    *
    * 'a' maps to ".-", 'b' maps to "-...", 'c' maps to "-.-.", and so on.
    *
    * For convenience, the full table for the 26 letters of the English alphabet is given below:
    *
    * [".-","-...","-.-.","-..",".","..-.","--.","....","..",".---","-.-",".-..","--","-.","---",".--.","--.-",".-.","...","-","..-","...-",".--","-..-","-.--","--.."]
    *
    * Given an array of strings words where each word can be written as a concatenation of the Morse
    * code of each letter.
    *
    * For example, "cab" can be written as "-.-..--...", which is the concatenation of "-.-.", ".-",
    * and "-...". We will call such a concatenation the transformation of a word.
    *
    * Return the number of different transformations among all words we have.
    *
    * Example 1:
    *
    * Input: words = ["gin","zen","gig","msg"] Output: 2 Explanation: The transformation of each
    * word is: "gin" -> "--...-." "zen" -> "--...-." "gig" -> "--...--." "msg" -> "--...--." There
    * are 2 different transformations: "--...-." and "--...--.".
    *
    * Example 2:
    *
    * Input: words = ["a"] Output: 1
    *
    * Constraints:
    *
    * 1 <= words.length <= 100 1 <= words[i].length <= 12 words[i] consists of lowercase English
    * letters.
    */
  def uniqueMorseRepresentations(words: Array[String]): Int =
    val dict = Array(
      ".-",
      "-...",
      "-.-.",
      "-..",
      ".",
      "..-.",
      "--.",
      "....",
      "..",
      ".---",
      "-.-",
      ".-..",
      "--",
      "-.",
      "---",
      ".--.",
      "--.-",
      ".-.",
      "...",
      "-",
      "..-",
      "...-",
      ".--",
      "-..-",
      "-.--",
      "--.."
    )
    words
      .map: word =>
        word.flatMap: char =>
          dict(char - 'a')
      .distinct
      .size

  /** 242. Valid anagram.
    *
    * Given two strings s and t, return true if t is an anagram of s, and false otherwise.
    *
    * An Anagram is a word or phrase formed by rearranging the letters of a different word or
    * phrase, typically using all the original letters exactly once.
    *
    * Example 1:
    *
    * Input: s = "anagram", t = "nagaram" Output: true
    *
    * Example 2:
    *
    * Input: s = "rat", t = "car" Output: false
    *
    * Constraints:
    *
    *   - 1 <= s.length, t.length <= 5 * 104 s
    *   - t consist of lowercase English letters.
    *
    * @note
    *   programming-skills
    *
    * ### Approach
    *   - Remember - anagram != palindrome.
    *   - Frquency of every char in given strings would match for an anagram.
    */
  def isAnagram(s: String, t: String): Boolean =
    if s.length == t.length then
      val sFrequency: Map[Char, Int] = s
        .groupBy(identity)
        .map: (char, string) =>
          (char, string.length)
      val tFrequency: Map[Char, Int] = t
        .groupBy(identity)
        .map: (char, string) =>
          (char, string.length)
      sFrequency == tFrequency
    else false

  /** 205. Isomorphic strings
    *
    * Given two strings s and t, determine if they are isomorphic.
    *
    * Two strings s and t are isomorphic if the characters in s can be replaced to get t.
    *
    * All occurrences of a character must be replaced with another character while preserving the
    * order of characters. No two characters may map to the same character, but a character may map
    * to itself.
    *
    * Example 1:
    *
    * Input: s = "egg", t = "add" Output: true
    *
    * Example 2:
    *
    * Input: s = "foo", t = "bar" Output: false
    *
    * Example 3:
    *
    * Input: s = "paper", t = "title" Output: true
    *
    * Constraints:
    *
    * 1 <= s.length <= 5 * 104 t.length == s.length s and t consist of any valid ascii character.
    */
  def isIsomorphic(s: String, t: String): Boolean =
    @scala.annotation.tailrec
    def compare(s: String, t: String, state: Map[Char, Char] = Map.empty[Char, Char]): Boolean =
      (s.headOption, t.headOption) match
        case (None, None)       => true
        case (Some(x), Some(y)) =>
          // println(s"x: $x, y:$y, state: ${state.mkString(",")}")
          if !state.contains(x) then
            if !state.values.toSeq.contains(y) then compare(s.tail, t.tail, state + (x -> y))
            else false
          else state(x) == y && compare(s.tail, t.tail, state)
        case _ => compare(s.tail, t.tail, state)
    // println(s"$s vs $t")
    s.length == t.length && compare(s, t, Map.empty[Char, Char])

  /** 412. Fizz Buzz
    *
    * Given an integer n, return a string array answer (1-indexed) where:
    *
    * answer[i] == "FizzBuzz" if i is divisible by 3 and 5. answer[i] == "Fizz" if i is divisible by
    * 3. answer[i] == "Buzz" if i is divisible by 5. answer[i] == i (as a string) if none of the
    * above conditions are true.
    *
    * Example 1:
    *
    * Input: n = 3 Output: ["1","2","Fizz"]
    *
    * Example 2:
    *
    * Input: n = 5 Output: ["1","2","Fizz","4","Buzz"]
    *
    * Example 3:
    *
    * Input: n = 15 Output:
    * ["1","2","Fizz","4","Buzz","Fizz","7","8","Fizz","Buzz","11","Fizz","13","14","FizzBuzz"]
    *
    * Constraints:
    *
    * 1 <= n <= 104
    */
  def fizzBuzz(n: Int): List[String] =
    (1 to n)
      .map: n =>
        n match
          case (e) if e % 15 == 0 => "FizzBuzz"
          case (e) if e % 5 == 0  => "Buzz"
          case (e) if e % 3 == 0  => "Fizz"
          case e                  => e.toString
      .toList

  /** 387. First unique character in a string.
    *
    * Given a string s, find the first non-repeating character in it and return its index. If it
    * does not exist, return -1.
    *
    * Example 1:
    *
    * Input: s = "leetcode" Output: 0
    *
    * Example 2:
    *
    * Input: s = "loveleetcode" Output: 2
    *
    * Example 3:
    *
    * Input: s = "aabb" Output: -1
    *
    * Constraints:
    *
    * 1 <= s.length <= 105 s consists of only lowercase English letters.
    */
  def firstUniqChar(s: String): Int =
    s.indexWhere(c => s.indexOf(c) == s.lastIndexOf(c))

  /** 1556. Thousand separator.
    *
    * Given an integer n, add a dot (".") as the thousands separator and return it in string format.
    *
    * Example 1:
    *
    * Input: n = 987 Output: "987"
    *
    * Example 2:
    *
    * Input: n = 1234 Output: "1.234"
    *
    * Constraints:
    *
    * 0 <= n <= 231 - 1
    */
  def thousandSeparator(n: Int): String =
    n.toString.reverse.grouped(3).mkString(".").reverse

  /** 1805. Number of different integers in a string
    *
    * You are given a string word that consists of digits and lowercase English letters.
    *
    * You will replace every non-digit character with a space. For example, "a123bc34d8ef34" will
    * become " 123 34 8 34". Notice that you are left with some integers that are separated by at
    * least one space: "123", "34", "8", and "34".
    *
    * Return the number of different integers after performing the replacement operations on word.
    *
    * Two integers are considered different if their decimal representations without any leading
    * zeros are different.
    *
    * Example 1:
    *
    * Input: word = "a123bc34d8ef34" Output: 3 Explanation: The three different integers are "123",
    * "34", and "8". Notice that "34" is only counted once.
    *
    * Example 2:
    *
    * Input: word = "leet1234code234" Output: 2
    *
    * Example 3:
    *
    * Input: word = "a1b01c001" Output: 1 Explanation: The three integers "1", "01", and "001" all
    * represent the same integer because the leading zeros are ignored when comparing their decimal
    * values.
    *
    * Constraints:
    *
    * 1 <= word.length <= 1000 word consists of digits and lowercase English letters.
    */
  def numDifferentIntegers(word: String): Int =
    val (uniqueInts, lastToken) = word.foldLeft(Set.empty[String], ""): (tuple, e) =>
      (tuple, e) match
        case ((acc, current @ "0"), '0')                  => (acc, current)
        case ((acc, "0"), element) if element.isDigit     => (acc, s"$element")
        case ((acc, current), element) if element.isDigit => (acc, s"$current$element")
        case ((acc, current), element) =>
          if current.isEmpty() then (acc, "") else ((acc + current), "")
    if lastToken.isEmpty() then uniqueInts.size else (uniqueInts + lastToken).size

  /** 1455. Check if a word occurs as a prefix of any word in a sentence.
    *
    * Given a sentence that consists of some words separated by a single space, and a searchWord,
    * check if searchWord is a prefix of any word in sentence.
    *
    * Return the index of the word in sentence (1-indexed) where searchWord is a prefix of this
    * word. If searchWord is a prefix of more than one word, return the index of the first word
    * (minimum index). If there is no such word return -1.
    *
    * A prefix of a string s is any leading contiguous substring of s.
    *
    * Example 1:
    *
    * Input: sentence = "i love eating burger", searchWord = "burg" Output: 4 Explanation: "burg" is
    * prefix of "burger" which is the 4th word in the sentence.
    *
    * Example 2:
    *
    * Input: sentence = "this problem is an easy problem", searchWord = "pro" Output: 2 Explanation:
    * "pro" is prefix of "problem" which is the 2nd and the 6th word in the sentence, but we return
    * 2 as it's the minimal index.
    *
    * Example 3:
    *
    * Input: sentence = "i am tired", searchWord = "you" Output: -1 Explanation: "you" is not a
    * prefix of any word in the sentence.
    *
    * Constraints:
    *
    * 1 <= sentence.length <= 100 1 <= searchWord.length <= 10 sentence consists of lowercase
    * English letters and spaces. searchWord consists of lowercase English letters.
    */
  def isPrefixOfWord(sentence: String, searchWord: String): Int =
    val mayBeFind = sentence
      .split("\\s+")
      .filter(!_.isEmpty())
      .zipWithIndex
      .find: (string, index) =>
        string.startsWith(searchWord)
    mayBeFind match
      case None             => -1
      case Some((_, index)) => index + 1

  /** 2185. Counting words with a given prefix.
    *
    * You are given an array of strings words and a string pref.
    *
    * Return the number of strings in words that contain pref as a prefix.
    *
    * A prefix of a string s is any leading contiguous substring of s.
    *
    * Example 1:
    *
    * Input: words = ["pay","attention","practice","attend"], pref = "at" Output: 2 Explanation: The
    * 2 strings that contain "at" as a prefix are: "attention" and "attend".
    *
    * Example 2:
    *
    * Input: words = ["leetcode","win","loops","success"], pref = "code" Output: 0 Explanation:
    * There are no strings that contain "code" as a prefix.
    *
    * Constraints: 1 <= words.length <= 100 1 <= words[i].length, pref.length <= 100 words[i] and
    * pref consist of lowercase English letters.
    */
  def prefixCount(words: Array[String], pref: String): Int = words.count(_.startsWith(pref))

  /** 2124. Check if all A's appear before All B's
    *
    * Given a string s consisting of only the characters 'a' and 'b', return true if every 'a'
    * appears before every 'b' in the string. Otherwise, return false.
    *
    * Example 1:
    *
    * Input: s = "aaabbb" Output: true Explanation: The 'a's are at indices 0, 1, and 2, while the
    * 'b's are at indices 3, 4, and 5. Hence, every 'a' appears before every 'b' and we return true.
    *
    * Example 2:
    *
    * Input: s = "abab" Output: false Explanation: There is an 'a' at index 2 and a 'b' at index 1.
    * Hence, not every 'a' appears before every 'b' and we return false.
    *
    * Example 3:
    *
    * Input: s = "bbb" Output: true Explanation: There are no 'a's, hence, every 'a' appears before
    * every 'b' and we return true.
    *
    * Constraints:
    *
    * 1 <= s.length <= 100
    *
    * s[i] is either 'a' or 'b'.
    */
  def checkString(s: String): Boolean =
    // println(s"$s, a:${s.lastIndexOf('a')}, b:${s.indexOf('b')}")
    !s.contains('a') || !s.contains('b') || s.lastIndexOf('a') < s.indexOf('b')

  /** 500. Keyboard row
    *
    * Given an array of strings words, return the words that can be typed using letters of the
    * alphabet on only one row of American keyboard like the image below.
    *
    * In the American keyboard:
    *
    * the first row consists of the characters "qwertyuiop", the second row consists of the
    * characters "asdfghjkl", and the third row consists of the characters "zxcvbnm".
    *
    * Example 1:
    *
    * Input: words = ["Hello","Alaska","Dad","Peace"] Output: ["Alaska","Dad"]
    *
    * Example 2:
    *
    * Input: words = ["omk"] Output: []
    *
    * Example 3:
    *
    * Input: words = ["adsdf","sfd"] Output: ["adsdf","sfd"]
    *
    * Constraints:
    *
    * 1 <= words.length <= 20 1 <= words[i].length <= 100 words[i] consists of English letters (both
    * lowercase and uppercase).
    */
  def findWords(words: Array[String]): Array[String] =
    val firstRow = "qwertyuiop".toSet
    val secondRow = "asdfghjkl".toSet
    val thirdRow = "zxcvbnm".toSet
    words.filter: thisword =>
      val word = thisword.toLowerCase
      word.forall(firstRow.contains(_)) ||
      word.forall(secondRow.contains(_)) ||
      word.forall(thirdRow.contains(_))

  /** 2423. Remove letter to equalize frequency.
    *
    * You are given a 0-indexed string word, consisting of lowercase English letters. You need to
    * select one index and remove the letter at that index from word so that the frequency of every
    * letter present in word is equal.
    *
    * Return true if it is possible to remove one letter so that the frequency of all letters in
    * word are equal, and false otherwise.
    *
    * Note:
    *
    * The frequency of a letter x is the number of times it occurs in the string. You must remove
    * exactly one letter and cannot choose to do nothing.
    *
    * Example 1:
    *
    *   - Input: word = "abcc"
    *   - Output: true
    *   - Explanation: Select index 3 and delete it: word becomes "abc" and each character has a
    *     frequency of 1.
    *
    * Example 2:
    *
    *   - Input: word = "aazz"
    *   - Output: false
    *   - Explanation: We must delete a character, so either the frequency of "a" is 1 and the
    *     frequency of "z" is 2, or vice versa. It is impossible to make all present letters have
    *     equal frequency.
    *
    * Constraints:
    *
    *   - 2 <= word.length <= 100
    *   - word consists of lowercase English letters only.
    */
  def equalFrequency(word: String): Boolean =
    lazy val isEqualFrequency: String => Boolean = word =>
      word
        .groupBy(identity)
        .map: (char, string) =>
          (char, string.size)
        .map(_._2)
        .toSet
        .size == 1

    (for i <- 0 until word.length
    yield word.take(i).concat(word.substring(i + 1)))
      .exists(isEqualFrequency)

  /** 3110. Score of a string.
    *
    * You are given a string s. The score of a string is defined as the sum of the absolute
    * difference between the ASCII values of adjacent characters.
    *
    * Return the score of s.
    *
    * Example 1:
    *
    * Input: s = "hello"
    *
    * Output: 13
    *
    * Explanation:
    *
    * The ASCII values of the characters in s are: 'h' = 104, 'e' = 101, 'l' = 108, 'o' = 111. So,
    * the score of s would be |104 - 101| + |101 - 108| + |108 - 108| + |108 - 111| = 3 + 7 + 0 + 3
    * \= 13.
    *
    * Example 2:
    *
    * Input: s = "zaz"
    *
    * Output: 50
    *
    * Explanation:
    *
    * The ASCII values of the characters in s are: 'z' = 122, 'a' = 97. So, the score of s would be
    * \|122 - 97| + |97 - 122| = 25 + 25 = 50.
    *
    * Constraints:
    *
    * 2 <= s.length <= 100 s consists only of lowercase English letters.
    */
  def scoreOfString(s: String): Int =
    (for i <- 0 to s.length - 2
    yield math.abs(s(i) - s(i + 1))).sum

  /** 2194. Cells in a range on an excel sheet.
    *
    * A cell (r, c) of an excel sheet is represented as a string "<col><row>" where:
    *
    * <col> denotes the column number c of the cell. It is represented by alphabetical letters. For
    * example, the 1st column is denoted by 'A', the 2nd by 'B', the 3rd by 'C', and so on. <row> is
    * the row number r of the cell. The rth row is represented by the integer r.
    *
    * You are given a string s in the format "<col1><row1>:<col2><row2>", where <col1> represents
    * the column c1, <row1> represents the row r1, <col2> represents the column c2, and <row2>
    * represents the row r2, such that r1 <= r2 and c1 <= c2.
    *
    * Return the list of cells (x, y) such that r1 <= x <= r2 and c1 <= y <= c2. The cells should be
    * represented as strings in the format mentioned above and be sorted in non-decreasing order
    * first by columns and then by rows.
    *
    * Example 1: Input: s = "K1:L2" Output: ["K1","K2","L1","L2"]
    *
    * Input: s = "A1:F1" Output: ["A1","B1","C1","D1","E1","F1"] Explanation: The above diagram
    * shows the cells which should be present in the list. The red arrow denotes the order in which
    * the cells should be presented.
    *
    * Constraints:
    *
    * s.length == 5 'A' <= s[0] <= s[3] <= 'Z' '1' <= s[1] <= s[4] <= '9' s consists of uppercase
    * English letters, digits and ':'.
    */
  def cellsInRange(s: String): List[String] =
    assert(s.length == 5)
    val cs = s(0)
    val ce = s(3)
    val rs = s(1)
    val re = s(4)
    (for
      c <- cs to ce
      r <- rs to re
    yield s"$c$r").toList

  /** 824. Goat Latin.
    *
    * You are given a string sentence that consist of words separated by spaces. Each word consists
    * of lowercase and uppercase letters only.
    *
    * We would like to convert the sentence to "Goat Latin" (a made-up language similar to Pig
    * Latin.) The rules of Goat Latin are as follows:
    *
    * If a word begins with a vowel ('a', 'e', 'i', 'o', or 'u'), append "ma" to the end of the
    * word. For example, the word "apple" becomes "applema". If a word begins with a consonant
    * (i.e., not a vowel), remove the first letter and append it to the end, then add "ma". For
    * example, the word "goat" becomes "oatgma". Add one letter 'a' to the end of each word per its
    * word index in the sentence, starting with 1. For example, the first word gets "a" added to the
    * end, the second word gets "aa" added to the end, and so on.
    *
    * Return the final sentence representing the conversion from sentence to Goat Latin.
    *
    * Example 1:
    *
    * Input: sentence = "I speak Goat Latin" Output: "Imaa peaksmaaa oatGmaaaa atinLmaaaaa"
    *
    * Example 2:
    *
    * Input: sentence = "The quick brown fox jumped over the lazy dog" Output: "heTmaa uickqmaaa
    * rownbmaaaa oxfmaaaaa umpedjmaaaaaa overmaaaaaaa hetmaaaaaaaa azylmaaaaaaaaa ogdmaaaaaaaaaa"
    *
    * Constraints:
    *
    * 1 <= sentence.length <= 150 sentence consists of English letters and spaces. sentence has no
    * leading or trailing spaces. All the words in sentence are separated by a single space.
    */
  def toGoatLatin(sentence: String): String =
    val transformer: String => String = s =>
      val vowels = Set('a', 'e', 'i', 'o', 'u')
      if vowels.contains(s.head.toLower) then s"${s}ma"
      else s"${s.tail}${s.head}ma"
    (for (word, index) <- sentence.split("\\s+").zipWithIndex if !word.isEmpty()
    yield transformer(word).concat("a" * (index + 1))).mkString(" ")

  /** 1002. Find common characters.
    *
    * Given a string array words, return an array of all characters that show up in all strings
    * within the words (including duplicates). You may return the answer in any order.
    *
    * Example 1:
    *
    * Input: words = ["bella","label","roller"] Output: ["e","l","l"]
    *
    * Example 2:
    *
    * Input: words = ["cool","lock","cook"] Output: ["c","o"]
    *
    * Constraints:
    *
    * 1 <= words.length <= 100 1 <= words[i].length <= 100 words[i] consists of lowercase English
    * letters.
    */
  def commonChars(words: Array[String]): List[String] =
    def common(a: String, b: String): String =
      a.toList.intersect(b).mkString
    words.reduce(common(_, _)).toList.map(_.toString)

  /** 2716. Minimize string length.
    *
    * Given a 0-indexed string s, repeatedly perform the following operation any number of times:
    *
    * Choose an index i in the string, and let c be the character in position i. Delete the closest
    * occurrence of c to the left of i (if any) and the closest occurrence of c to the right of i
    * (if any).
    *
    * Your task is to minimize the length of s by performing the above operation any number of
    * times.
    *
    * Return an integer denoting the length of the minimized string.
    *
    * Example 1:
    *
    * Input: s = "aaabc" Output: 3 Explanation: In this example, s is "aaabc". We can start by
    * selecting the character 'a' at index 1. We then remove the closest 'a' to the left of index 1,
    * which is at index 0, and the closest 'a' to the right of index 1, which is at index 2. After
    * this operation, the string becomes "abc". Any further operation we perform on the string will
    * leave it unchanged. Therefore, the length of the minimized string is 3.
    *
    * Example 2:
    *
    * Input: s = "cbbd" Output: 3 Explanation: For this we can start with character 'b' at index 1.
    * There is no occurrence of 'b' to the left of index 1, but there is one to the right at index
    * 2, so we delete the 'b' at index 2. The string becomes "cbd" and further operations will leave
    * it unchanged. Hence, the minimized length is 3.
    *
    * Example 3:
    *
    * Input: s = "dddaaa" Output: 2 Explanation: For this, we can start with the character 'd' at
    * index 1. The closest occurrence of a 'd' to its left is at index 0, and the closest occurrence
    * of a 'd' to its right is at index 2. We delete both index 0 and 2, so the string becomes
    * "daaa". In the new string, we can select the character 'a' at index 2. The closest occurrence
    * of an 'a' to its left is at index 1, and the closest occurrence of an 'a' to its right is at
    * index 3. We delete both of them, and the string becomes "da". We cannot minimize this further,
    * so the minimized length is 2.
    *
    * Constraints:
    *
    * 1 <= s.length <= 100 s contains only lowercase English letters
    */
  def minimizedStringLength(s: String): Int = s.distinct.size

  /** 1309. Decrypt string from alphabet to integer mapping.
    *
    * You are given a string s formed by digits and '#'. We want to map s to English lowercase
    * characters as follows:
    *
    * Characters ('a' to 'i') are represented by ('1' to '9') respectively. Characters ('j' to 'z')
    * are represented by ('10#' to '26#') respectively.
    *
    * Return the string formed after mapping.
    *
    * The test cases are generated so that a unique mapping will always exist.
    *
    * Example 1:
    *
    * Input: s = "10#11#12" Output: "jkab" Explanation: "j" -> "10#" , "k" -> "11#" , "a" -> "1" ,
    * "b" -> "2".
    *
    * Example 2:
    *
    * Input: s = "1326#" Output: "acz"
    *
    * Constraints:
    *
    * 1 <= s.length <= 1000 s consists of digits and the '#' letter. s will be a valid string such
    * that mapping is always possible.
    */
  def freqAlphabets(s: String): String =
    val asChar: String => String = string => (string.toInt - 1 + 'a').toChar.toString
    @scala.annotation.tailrec
    def loop(chars: List[Char], current: String, acc: String): String =
      chars match
        case head @ '#' :: next =>
          val (singles, double) = current.splitAt(current.length - 2)
          val allSingles = singles.collect(c => asChar(c.toString)).mkString
          val doubleChar = asChar(double)
          val newAcc = s"$acc$allSingles$doubleChar"
          loop(next, "", newAcc)
        case head :: next =>
          loop(next, s"$current$head", acc)
        case Nil =>
          val allSingles = current.collect(c => asChar(c.toString)).mkString
          val newAcc = s"$acc$allSingles"
          newAcc
    loop(s.toCharArray().toList, current = "", acc = "")

  /** 1844. Replace all digits with characters.
    *
    * You are given a 0-indexed string s that has lowercase English letters in its even indices and
    * digits in its odd indices.
    *
    * There is a function shift(c, x), where c is a character and x is a digit, that returns the xth
    * character after c.
    *
    * For example, shift('a', 5) = 'f' and shift('x', 0) = 'x'.
    *
    * For every odd index i, you want to replace the digit s[i] with shift(s[i-1], s[i]).
    *
    * Return s after replacing all digits. It is guaranteed that shift(s[i-1], s[i]) will never
    * exceed 'z'.
    *
    * Example 1:
    *
    * Input: s = "a1c1e1" Output: "abcdef" Explanation: The digits are replaced as follows: \- s[1]
    * -> shift('a',1) = 'b' \- s[3] -> shift('c',1) = 'd' \- s[5] -> shift('e',1) = 'f'
    *
    * Example 2:
    *
    * Input: s = "a1b2c3d4e" Output: "abbdcfdhe" Explanation: The digits are replaced as follows: \-
    * s[1] -> shift('a',1) = 'b' \- s[3] -> shift('b',2) = 'd' \- s[5] -> shift('c',3) = 'f' \- s[7]
    * -> shift('d',4) = 'h'
    *
    * Constraints:
    *
    * 1 <= s.length <= 100 s consists only of lowercase English letters and digits. shift(s[i-1],
    * s[i]) <= 'z' for all odd indices i.
    */
  def replaceDigits(s: String): String =
    (for index <- 0 until s.length by 2
    yield
      val a = s(index)
      if index < s.length - 1 then
        val b = s(index + 1)
        val n = (a + b.asDigit).toChar
        s"$a$n"
      else s"$a"
    ).mkString

  /** 2744. Find maximum number of string pairs
    *
    * You are given a 0-indexed array words consisting of distinct strings.
    *
    * The string words[i] can be paired with the string words[j] if:
    *
    * The string words[i] is equal to the reversed string of words[j]. 0 <= i < j < words.length.
    *
    * Return the maximum number of pairs that can be formed from the array words.
    *
    * Note that each string can belong in at most one pair.
    *
    * Example 1:
    *
    * Input: words = ["cd","ac","dc","ca","zz"] Output: 2 Explanation: In this example, we can form
    * 2 pair of strings in the following way: \- We pair the 0th string with the 2nd string, as the
    * reversed string of word[0] is "dc" and is equal to words[2]. \- We pair the 1st string with
    * the 3rd string, as the reversed string of word[1] is "ca" and is equal to words[3]. It can be
    * proven that 2 is the maximum number of pairs that can be formed.
    *
    * Example 2:
    *
    * Input: words = ["ab","ba","cc"] Output: 1 Explanation: In this example, we can form 1 pair of
    * strings in the following way: \- We pair the 0th string with the 1st string, as the reversed
    * string of words[1] is "ab" and is equal to words[0]. It can be proven that 1 is the maximum
    * number of pairs that can be formed.
    *
    * Example 3:
    *
    * Input: words = ["aa","ab"] Output: 0 Explanation: In this example, we are unable to form any
    * pair of strings.
    *
    * Constraints:
    *
    * 1 <= words.length <= 50 words[i].length == 2 words consists of distinct strings. words[i]
    * contains only lowercase English letters.
    */
  def maximumNumberOfStringPairs(words: Array[String]): Int =
    words.map(_.sorted).groupBy(identity).map(_._2.length / 2).sum

  /** 13. Roman to integer.
    *
    * Roman numerals are represented by seven different symbols: I, V, X, L, C, D and M.
    *
    * Symbol Value I 1 V 5 X 10 L 50 C 100 D 500 M 1000
    *
    * For example, 2 is written as II in Roman numeral, just two ones added together. 12 is written
    * as XII, which is simply X + II. The number 27 is written as XXVII, which is XX + V + II.
    *
    * Roman numerals are usually written largest to smallest from left to right. However, the
    * numeral for four is not IIII. Instead, the number four is written as IV. Because the one is
    * before the five we subtract it making four. The same principle applies to the number nine,
    * which is written as IX. There are six instances where subtraction is used:
    *
    * I can be placed before V (5) and X (10) to make 4 and 9. X can be placed before L (50) and C
    * (100) to make 40 and 90. C can be placed before D (500) and M (1000) to make 400 and 900.
    *
    * Given a roman numeral, convert it to an integer.
    *
    * Example 1:
    *
    *   - Input: s = "III"
    *   - Output: 3
    *   - Explanation: III = 3.
    *
    * Example 2:
    *
    *   - Input: s = "LVIII"
    *   - Output: 58
    *   - Explanation: L = 50, V= 5, III = 3.
    *
    * Example 3:
    *
    *   - Input: s = "MCMXCIV"
    *   - Output: 1994
    *   - Explanation: M = 1000, CM = 900, XC = 90 and IV = 4.
    *
    * Constraints:
    *
    *   - 1 <= s.length <= 15
    *   - s contains only the characters ('I', 'V', 'X', 'L', 'C', 'D', 'M').
    *   - It is guaranteed that s is a valid roman numeral in the range [1, 3999].
    *
    * @note
    *   programming-skills
    */
  def romanToInt(s: String): Int =
    lazy val fromRomanToDecimal: Map[Char, Int] = Map(
      ('I' -> 1),
      ('V' -> 5),
      ('X' -> 10),
      ('L' -> 50),
      ('C' -> 100),
      ('D' -> 500),
      ('M' -> 1000)
    )
    // ("MCMXCIV", 1994)
    val (int, x) = s.foldRight(0, 0): (element, tuple) =>
      (element, tuple) match
        case (element, (acc, valueToRight)) if fromRomanToDecimal(element) < valueToRight =>
          (acc - fromRomanToDecimal(element), fromRomanToDecimal(element))
        case (element, (acc, valueToRight)) =>
          (acc + fromRomanToDecimal(element), fromRomanToDecimal(element))
    int

  /** 1417. Reformat the string.
    *
    * You are given an alphanumeric string s. (Alphanumeric string is a string consisting of
    * lowercase English letters and digits).
    *
    * You have to find a permutation of the string where no letter is followed by another letter and
    * no digit is followed by another digit. That is, no two adjacent characters have the same type.
    *
    * Return the reformatted string or return an empty string if it is impossible to reformat the
    * string.
    *
    * Example 1:
    *
    * Input: s = "a0b1c2" Output: "0a1b2c" Explanation: No two adjacent characters have the same
    * type in "0a1b2c". "a0b1c2", "0a1b2c", "0c2a1b" are also valid permutations.
    *
    * Example 2:
    *
    * Input: s = "leetcode" Output: "" Explanation: "leetcode" has only characters so we cannot
    * separate them by digits.
    *
    * Example 3:
    *
    * Input: s = "1229857369" Output: "" Explanation: "1229857369" has only digits so we cannot
    * separate them by characters.
    *
    * Constraints:
    *
    * 1 <= s.length <= 500 s consists of only lowercase English letters and/or digits.
    */
  def reformat(s: String): String =
    val (chars, ints) = s.foldLeft(List.empty[Char], List.empty[Int]): (tuple, element) =>
      (tuple, element) match
        case ((chars, ints), element) if element.isDigit => (chars, element.asDigit +: ints)
        case ((chars, ints), element)                    => (element +: chars, ints)
    if math.abs(chars.length - ints.length) > 1 then ""
    else
      (if chars.length > ints.length then chars.zipAll(ints, ' ', ' ')
       else ints.zipAll(chars, ' ', ' '))
        .map(t => s"${t._1}${t._2}")
        .mkString
        .trim

  /** 345. Reverse vowels of a string.
    *
    * Given a string s, reverse only all the vowels in the string and return it.
    *
    * The vowels are 'a', 'e', 'i', 'o', and 'u', and they can appear in both lower and upper cases,
    * more than once.
    *
    * Example 1:
    *
    * Input: s = "hello" Output: "holle"
    *
    * Example 2:
    *
    * Input: s = "leetcode" Output: "leotcede"
    *
    * Constraints:
    *
    * 1 <= s.length <= 3 * 105 s consist of printable ASCII characters.
    */
  def reverseVowels(s: String): String =
    lazy val vowels = Set('a', 'e', 'i', 'o', 'u')
    @scala.annotation.tailrec
    def loop(chars: Array[Char], l: Int, r: Int): String =
      if l < r then
        (s(l), s(r)) match
          case (x, y) if !vowels.contains(x.toLower) && !vowels.contains(y.toLower) =>
            loop(chars, l + 1, r - 1)
          case (x, y) if vowels.contains(x.toLower) && !vowels.contains(y.toLower) =>
            loop(chars, l, r - 1)
          case (x, y) if !vowels.contains(x.toLower) && vowels.contains(y.toLower) =>
            loop(chars, l + 1, r)
          case (x, y) =>
            chars(l) = (chars(l) + chars(r)).toChar
            chars(r) = (chars(l) - chars(r)).toChar
            chars(l) = (chars(l) - chars(r)).toChar
            loop(chars, l + 1, r - 1)
      else chars.mkString

    loop(s.toCharArray(), 0, s.length - 1)

  /** 1941. Check if all characters have equal number of occurences.
    *
    * Given a string s, return true if s is a good string, or false otherwise.
    *
    * A string s is good if all the characters that appear in s have the same number of occurrences
    * (i.e., the same frequency).
    *
    * Example 1:
    *
    * Input: s = "abacbc" Output: true Explanation: The characters that appear in s are 'a', 'b',
    * and 'c'. All characters occur 2 times in s.
    *
    * Example 2:
    *
    * Input: s = "aaabb" Output: false Explanation: The characters that appear in s are 'a' and 'b'.
    * 'a' occurs 3 times while 'b' occurs 2 times, which is not the same number of times.
    *
    * Constraints:
    *
    * 1 <= s.length <= 1000 s consists of lowercase English letters.
    */
  def areOccurrencesEqual(s: String): Boolean =
    s.groupBy(identity).map(_._2.size).toSet.size == 1

  /** 2315. Count asterisks.
    *
    * You are given a string s, where every two consecutive vertical bars '|' are grouped into a
    * pair. In other words, the 1st and 2nd '|' make a pair, the 3rd and 4th '|' make a pair, and so
    * forth.
    *
    * Return the number of '*' in s, excluding the '*' between each pair of '|'.
    *
    * Note that each '|' will belong to exactly one pair.
    *
    * Example 1:
    *
    * Input: s = "l|*e*et|c**o|*de|" Output: 2 Explanation: The considered characters are
    * underlined: "l|*e*et|c**o|*de|". The characters between the first and second '|' are excluded
    * from the answer. Also, the characters between the third and fourth '|' are excluded from the
    * answer. There are 2 asterisks considered. Therefore, we return 2.
    *
    * Example 2:
    *
    * Input: s = "iamprogrammer" Output: 0 Explanation: In this example, there are no asterisks in
    * s. Therefore, we return 0.
    *
    * Example 3:
    *
    * Input: s = "yo|uar|e**|b|e***au|tifu|l" Output: 5 Explanation: The considered characters are
    * underlined: "yo|uar|e**|b|e***au|tifu|l". There are 5 asterisks considered. Therefore, we
    * return 5.
    *
    * Constraints:
    *
    * 1 <= s.length <= 1000 s consists of lowercase English letters, vertical bars '|', and
    * asterisks '*'.
    *
    * s contains an even number of vertical bars '|'.
    *
    * ### Approach:
    *
    * \- Parse from left to right \- Starting with out = true, keep toggling on every |. \- When out
    * \== true, means context is outside the pair. So, accumulate counter of *StringProblems
    */
  def countAsterisks(s: String): Int =
    val (counter, isOutside) =
      s.foldLeft(0, true): (tuple, char) =>
        (tuple, char) match
          case ((counter, out), '*')  => if out then (counter + 1, out) else (counter, out)
          case ((counter, out), '|')  => (counter, !out)
          case ((counter, out), char) => (counter, out)
    counter

  /** 2255. Count prefixes of a given string.
    *
    * You are given a string array words and a string s, where words[i] and s comprise only of
    * lowercase English letters.
    *
    * Return the number of strings in words that are a prefix of s.
    *
    * A prefix of a string is a substring that occurs at the beginning of the string. A substring is
    * a contiguous sequence of characters within a string.
    *
    * Example 1:
    *
    * Input: words = ["a","b","c","ab","bc","abc"], s = "abc" Output: 3 Explanation: The strings in
    * words which are a prefix of s = "abc" are: "a", "ab", and "abc". Thus the number of strings in
    * words which are a prefix of s is 3.
    *
    * Example 2:
    *
    * Input: words = ["a","a"], s = "aa" Output: 2 Explanation: Both of the strings are a prefix of
    * s. Note that the same string can occur multiple times in words, and it should be counted each
    * time.
    *
    * Constraints:
    *
    *   - 1 <= words.length <= 1000 1 <= words[i].length,
    *   - s.length <= 10 words[i] and s consist of * lowercase English letters only.
    */
  def countPrefixes(words: Array[String], s: String): Int =
    words.count(s.startsWith)

  /** 2278. Percentage of letter in string.
    *
    * Given a string s and a character letter, return the percentage of characters in s that equal
    * letter rounded down to the nearest whole percent.
    *
    * Example 1:
    *
    * Input: s = "foobar", letter = "o" Output: 33 Explanation: The percentage of characters in s
    * that equal the letter 'o' is 2 / 6 * 100% = 33% when rounded down, so we return 33.
    *
    * Example 2:
    *
    * Input: s = "jjjj", letter = "k" Output: 0 Explanation: The percentage of characters in s that
    * equal the letter 'k' is 0%, so we return 0.
    *
    * Constraints:
    *
    * 1 <= s.length <= 100 s consists of lowercase English letters. letter is a lowercase English
    * letter.
    */

  def percentageLetter(s: String, letter: Char): Int =
    s.count(_ == letter) * 100 / s.length

  /** 1071. Greatest common divisor.
    *
    * For two strings s and t, we say "t divides s" if and only if s = t + t + t + ... + t + t
    * (i.e., t is concatenated with itself one or more times).
    *
    * Given two strings str1 and str2, return the largest string x such that x divides both str1 and
    * str2.
    *
    * Example 1:
    *
    * Input: str1 = "ABCABC", str2 = "ABC" Output: "ABC"
    *
    * Example 2:
    *
    * Input: str1 = "ABABAB", str2 = "ABAB" Output: "AB"
    *
    * Example 3:
    *
    * Input: str1 = "LEET", str2 = "CODE" Output: ""
    *
    * Constraints:
    *
    *   - 1 <= str1.length, str2.length <= 1000
    *   - str1 and str2 consist of English uppercase letters.
    *
    * ### Approach
    *   - Assume one of the given strings to be smaller and other one to be larger, say (x, y)
    *     respectively.
    *   - For smaller to be a divisor of larger, the larger one should start with smaller one.
    *   - Substring the larger one by the smaller until either the last two undivided units are
    *     either equal or unequal.
    */
  def gcdOfStrings(str1: String, str2: String): String =
    (str1, str2) match
      case (x, y) if x == y              => x
      case (x, y) if x.length > y.length => gcdOfStrings(y, x)
      case (x, y) if y.startsWith(x)     => gcdOfStrings(y.substring(x.length), x)
      case _                             => ""

  /** 151. Reverse words in a string.
    *
    * Given an input string s, reverse the order of the words.
    *
    * A word is defined as a sequence of non-space characters. The words in s will be separated by
    * at least one space.
    *
    * Return a string of the words in reverse order concatenated by a single space.
    *
    * Note that s may contain leading or trailing spaces or multiple spaces between two words. The
    * returned string should only have a single space separating the words. Do not include any extra
    * spaces.
    *
    * Example 1:
    *
    * Input: s = "the sky is blue" Output: "blue is sky the"
    *
    * Example 2:
    *
    * Input: s = " hello world " Output: "world hello" Explanation: Your reversed string should not
    * contain leading or trailing spaces.
    *
    * Example 3:
    *
    * Input: s = "a good example" Output: "example good a" Explanation: You need to reduce multiple
    * spaces between two words to a single space in the reversed string.
    *
    * Constraints:
    *
    *   - 1 <= s.length <= 104
    *   - s contains English letters (upper-case and lower-case), digits, and * spaces ' '.
    *   - There is at least one word in s.
    *
    * Follow-up: If the string data type is mutable in your language, can you solve it in-place with
    * O(1) extra space?
    */
  def reverseWords(s: String): String =
    s.split(" ").filter(!_.isEmpty()).reverse.mkString(" ")

  /** 443. String compression.
    *
    * Given an array of characters chars, compress it using the following algorithm:
    *
    * Begin with an empty string s. For each group of consecutive repeating characters in chars:
    *
    * If the group's length is 1, append the character to s. Otherwise, append the character
    * followed by the group's length.
    *
    * The compressed string s should not be returned separately, but instead, be stored in the input
    * character array chars. Note that group lengths that are 10 or longer will be split into
    * multiple characters in chars.
    *
    * After you are done modifying the input array, return the new length of the array.
    *
    * You must write an algorithm that uses only constant extra space.
    *
    * Example 1:
    *
    * Input: chars = ["a","a","b","b","c","c","c"] Output: Return 6, and the first 6 characters of
    * the input array should be: ["a","2","b","2","c","3"] Explanation: The groups are "aa", "bb",
    * and "ccc". This compresses to "a2b2c3".
    *
    * Example 2:
    *
    * Input: chars = ["a"] Output: Return 1, and the first character of the input array should be:
    * ["a"] Explanation: The only group is "a", which remains uncompressed since it's a single
    * character.
    *
    * Example 3:
    *
    * Input: chars = ["a","b","b","b","b","b","b","b","b","b","b","b","b"] Output: Return 4, and the
    * first 4 characters of the input array should be: ["a","b","1","2"]. Explanation: The groups
    * are "a" and "bbbbbbbbbbbb". This compresses to "ab12".
    *
    * Constraints:
    *
    * 1 <= chars.length <= 2000 chars[i] is a lowercase English letter, uppercase English letter,
    * digit, or symbol.
    */

  def compress(chars: Array[Char]): Int =
    @scala.annotation.tailrec
    def iterate(chars: Array[Char], prevChar: Char, prevCharCounter: Int, acc: String): String =
      chars.headOption match
        case Some(char) =>
          if prevChar == char then
            iterate(chars.tail, prevChar = prevChar, prevCharCounter = prevCharCounter + 1, acc)
          else
            val c = if prevCharCounter > 1 then s"$prevChar$prevCharCounter" else s"$prevChar"
            iterate(chars.tail, char, 1, s"$acc$c")
        case None =>
          val c = if prevCharCounter > 1 then s"$prevChar$prevCharCounter" else s"$prevChar"
          s"$acc$c"
    val compressedString: String = iterate(chars.tail, chars.head, 1, "")
    for i <- compressedString.indices
    do chars(i) = compressedString(i)
    compressedString.length

  /** 2351. First letter to appear twice.
    *
    * Given a string s consisting of lowercase English letters, return the first letter to appear
    * twice.
    *
    * Note:
    *
    * A letter a appears twice before another letter b if the second occurrence of a is before the
    * second occurrence of b. s will contain at least one letter that appears twice.
    *
    * Example 1:
    *
    * Input: s = "abccbaacz" Output: "c" Explanation: The letter 'a' appears on the indexes 0, 5 and
    * 6. The letter 'b' appears on the indexes 1 and 4. The letter 'c' appears on the indexes 2, 3
    * and 7. The letter 'z' appears on the index 8. The letter 'c' is the first letter to appear
    * twice, because out of all the letters the index of its second occurrence is the smallest.
    *
    * Example 2:
    *
    * Input: s = "abcdd" Output: "d" Explanation: The only letter that appears twice is 'd' so we
    * return 'd'.
    *
    * Constraints:
    *
    * 2 <= s.length <= 100 s consists of lowercase English letters.
    *
    * s has at least one repeated letter.
    */
  def repeatedCharacter(s: String): Char =
    @scala.annotation.tailrec
    def loop(index: Int, visited: Set[Char]): Char =
      if visited.contains(s(index)) then s(index) else loop(index + 1, visited + s(index))
    loop(0, Set.empty[Char])

  /** 2788. Split strings by separator.
    *
    * Given an array of strings words and a character separator, split each string in words by
    * separator. Return an array of strings containing the new strings formed after the splits,
    * excluding empty strings.
    *
    * Notes
    *
    * separator is used to determine where the split should occur, but it is not included as part of
    * the resulting strings. A split may result in more than two strings. The resulting strings must
    * maintain the same order as they were initially given.
    *
    * Example 1:
    *
    *   - Input: words = ["one.two.three","four.five","six"], separator = "."
    *   - Output: ["one","two","three","four","five","six"]
    *   - Explanation: In this example we split as follows: "one.two.three" splits into "one",
    *     "two", "three" "four.five" splits into "four", "five" "six" splits into "six"
    *
    * Hence, the resulting array is ["one","two","three","four","five","six"].
    *
    * Example 2:
    *
    *   - Input: words = ["$easy$","$problem$"], separator = "$"
    *   - Output: ["easy","problem"]
    *   - Explanation: In this example we split as follows: "$easy$" splits into "easy" (excluding
    *     empty strings) "$problem$" splits into "problem" (excluding empty strings). Hence, the
    *     resulting array is ["easy","problem"].
    *
    * Example 3:
    *
    *   - Input: words = ["|||"], separator = "|"
    *   - Output: []
    *   - Explanation: In this example the resulting split of "|||" will contain only empty strings,
    *     so we return an empty array [].
    *
    * Constraints:
    *
    *   - 1 <= words.length <= 100
    *   - 1 <= words[i].length <= 20
    *   - characters in words[i] are either lowercase English letters or characters from the string
    *     ".,|$#@" (excluding the quotes) separator is a character from the string ".,|$#@"
    *     (excluding the quotes)
    */
  def splitWordsBySeparator(words: List[String], separator: Char): List[String] =
    words.map(_.split(separator)).flatMap(as => as.filter(!_.isEmpty()))

  /** 459. Repeated substring pattern.
    *
    * Given a string s, check if it can be constructed by taking a substring of it and appending
    * multiple copies of the substring together.
    *
    * Example 1:
    *
    *   - Input: s = "abab"
    *   - Output: true
    *   - Explanation: It is the substring "ab" twice.
    *
    * Example 2:
    *
    *   - Input: s = "aba"
    *   - Output: false
    *
    * Example 3:
    *
    *   - Input: s = "abcabcabcabc"
    *   - Output: true
    *   - Explanation: It is the substring "abc" four times or the substring "abcabc" twice.
    *
    * Constraints:
    *
    *   - 1 <= s.length <= 10^4
    *   - s consists of lowercase English letters.
    *
    * ### Approach
    *   - a substring is repeatable to form the larger string.
    *   - so length of subtring must be a factor of the length of the larger string.
    *   - 1 & s.length is also a factor.
    *   - If a subtring of length == s.length is the only repeatable substring then there is exists no other substring.
    *     - For example, in "ababab" - length is 6 and factors are 2 & 3.
    *     - answer lies in the set (of length =2) or aba (of length=3)
    */

  def repeatedSubstringPattern(s: String): Boolean =
    def factors(n: Int): Seq[Int] =
      (for i <- (1 to n / 2) if n % i == 0 yield Set(i, n / i)).flatten.distinct.sorted
    factors(s.length())
      .filter(_ != s.length)
      .exists: f =>
        s.substring(0, f) * (s.length / f) == s

  /** 657. Robot return to origin.
    *
    * There is a robot starting at the position (0, 0), the origin, on a 2D plane. Given a sequence of its moves, judge if this robot ends up at (0, 0) after it completes its moves.
    *
    * You are given a string moves that represents the move sequence of the robot where moves[i] represents its ith move. Valid moves are 'R' (right), 'L' (left), 'U' (up), and 'D' (down).
    *
    * Return true if the robot returns to the origin after it finishes all of its moves, or false otherwise.
    *
    * Note: The way that the robot is "facing" is irrelevant. 'R' will always make the robot move to the right once, 'L' will always make it move left, etc. Also, assume that the magnitude of the robot's movement is the same for each move.
    *
    * Example 1:
    *
    * - Input: moves = "UD"
    * - Output: true
    * - Explanation: The robot moves up once, and then down once. All moves have the same magnitude, so it ended up at the origin where it started. Therefore, we return true.
    *
    * Example 2:
    *
    * - Input: moves = "LL"
    * - Output: false
    * - Explanation: The robot moves left twice. It ends up two "moves" to the left of the origin. We return false because it is not at the origin at the end of its moves.
    *
    * Constraints:
    *
    *   - 1 <= moves.length <= 2 * 10^4
    *   - moves only contains the characters 'U', 'D', 'L' and 'R'.
    */
  def judgeCircle(moves: String): Boolean =
    moves.foldLeft((0, 0)): (state, move) =>
      move match
        case 'L' => (state._1 - 1, state._2)
        case 'R' => (state._1 + 1, state._2)
        case 'U' => (state._1, state._2 + 1)
        case 'D' => (state._1, state._2 - 1)
    == (0, 0)

  /** 1041. Robot bounded in a circle.
    *
    * On an infinite plane, a robot initially stands at (0, 0) and faces north. Note that:
    *
    *    The north direction is the positive direction of the y-axis.
    *    The south direction is the negative direction of the y-axis.
    *    The east direction is the positive direction of the x-axis.
    *    The west direction is the negative direction of the x-axis.
    *
    * The robot can receive one of three instructions:
    *
    *    "G": go straight 1 unit.
    *    "L": turn 90 degrees to the left (i.e., anti-clockwise direction).
    *    "R": turn 90 degrees to the right (i.e., clockwise direction).
    *
    * The robot performs the instructions given in order, and repeats them forever.
    *
    * Return true if and only if there exists a circle in the plane such that the robot never leaves the circle.
    *
    * Example 1:
    *
    * - Input: instructions = "GGLLGG"
    * - Output: true
    * - Explanation: The robot is initially at (0, 0) facing the north direction.
    *     "G": move one step. Position: (0, 1). Direction: North.
    *     "G": move one step. Position: (0, 2). Direction: North.
    *     "L": turn 90 degrees anti-clockwise. Position: (0, 2). Direction: West.
    *     "L": turn 90 degrees anti-clockwise. Position: (0, 2). Direction: South.
    *     "G": move one step. Position: (0, 1). Direction: South.
    *     "G": move one step. Position: (0, 0). Direction: South.
    * Repeating the instructions, the robot goes into the cycle: (0, 0) --> (0, 1) --> (0, 2) --> (0, 1) --> (0, 0).
    * Based on that, we return true.
    *
    * Example 2:
    *
    * - Input: instructions = "GG"
    * - Output: false
    * - Explanation: The robot is initially at (0, 0) facing the north direction.
    *     "G": move one step. Position: (0, 1). Direction: North.
    *     "G": move one step. Position: (0, 2). Direction: North.
    * Repeating the instructions, keeps advancing in the north direction and does not go into cycles.
    * Based on that, we return false.
    *
    * Example 3:
    *
    * - Input: instructions = "GL"
    * - Output: true
    * - Explanation: The robot is initially at (0, 0) facing the north direction.
    *     "G": move one step. Position: (0, 1). Direction: North.
    *     "L": turn 90 degrees anti-clockwise. Position: (0, 1). Direction: West.
    *     "G": move one step. Position: (-1, 1). Direction: West.
    *     "L": turn 90 degrees anti-clockwise. Position: (-1, 1). Direction: South.
    *     "G": move one step. Position: (-1, 0). Direction: South.
    *     "L": turn 90 degrees anti-clockwise. Position: (-1, 0). Direction: East.
    *     "G": move one step. Position: (0, 0). Direction: East.
    *     "L": turn 90 degrees anti-clockwise. Position: (0, 0). Direction: North.
    * Repeating the instructions, the robot goes into the cycle: (0, 0) --> (0, 1) --> (-1, 1) --> (-1, 0) --> (0, 0). Based on that, we return true.
    *
    * Constraints:
    *
    *    - 1 <= instructions.length <= 100
    *    - instructions[i] is 'G', 'L' or, 'R'.
    *
    * ### Approach
    * - After completing the instruction once, if the robot ends up facing north then it will never reach origin unless it has completed a circle.
    * - So, check if the robot should not be facing north for it to reach origin
    * - if facing north, check for equi distance traveled in every direction.
    */

  def isRobotBounded(instructions: String): Boolean =
    enum Directions(v: Int):
      case n extends Directions(0)
      case e extends Directions(1)
      case s extends Directions(2)
      case w extends Directions(3)

      def rotateClockwise: Directions =
        Directions.fromOrdinal((v + 1) % 4)

      def rotateCounterClockwise: Directions =
        Directions.fromOrdinal((v - 1 + 4) % 4)

    val (vector, map) =
      instructions.foldLeft(Directions.n, Map.empty[Directions, Int]): (tuple, instruction) =>
        (tuple, instruction) match
          case ((direction, map), 'L') => (direction.rotateClockwise, map)
          case ((direction, map), 'R') => (direction.rotateCounterClockwise, map)
          case ((direction, map), n) =>
            val move: (Directions, Int) = (direction -> (map.getOrElse(direction, 0) + 1))
            (direction, map + move)
    vector != Directions.n || (
      map.getOrElse(Directions.n, 0) == map.getOrElse(Directions.s, 0) &&
        map.getOrElse(Directions.e, 0) == map.getOrElse(Directions.w, 0)
    )

  /** 2073. Time needed to buy tickets.
    *
    * There are n people in a line queuing to buy tickets, where the 0th person is at the front of
    * the line and the (n - 1)th person is at the back of the line.
    *
    * You are given a 0-indexed integer array tickets of length n where the number of tickets that
    * the ith person would like to buy is tickets[i].
    *
    * Each person takes exactly 1 second to buy a ticket. A person can only buy 1 ticket at a time
    * and has to go back to the end of the line (which happens instantaneously) in order to buy more
    * tickets. If a person does not have any tickets left to buy, the person will leave the line.
    *
    * Return the time taken for the person at position k (0-indexed) to finish buying tickets.
    *
    * Example 1:
    *
    *   - Input: tickets = [2,3,2], k = 2
    *   - Output: 6
    *   - Explanation:
    *     - In the first pass, everyone in the line buys a ticket and the line becomes [1, 2, 1].
    *     - In the second pass, everyone in the line buys a ticket and the line becomes [0, 1, 0].
    *       The person at position 2 has successfully bought 2 tickets and it took 3 + 3 = 6
    *       seconds.
    *
    * Example 2:
    *
    *   - Input: tickets = [5,1,1,1], k = 0
    *   - Output: 8
    *   - Explanation:
    *     - In the first pass, everyone in the line buys a ticket and the line becomes [4, 0, 0, 0].
    *     - In the next 4 passes, only the person in position 0 is buying tickets. The person at
    *       position 0 has successfully bought 5 tickets and it took 4 + 1 + 1 + 1 + 1 = 8 seconds.
    *
    * Constraints:
    *
    *   - n == tickets.length
    *   - 1 <= n <= 100
    *   - 1 <= tickets[i] <= 100
    *   - 0 <= k < n
    *
    * ##### Approach
    *
    * Solution to be approached in a single pass Every ticket bought is ONE second. Question is to
    * find the time required. Time required will be equal to the number of tickets sold until kth
    * person has no more tickets to buy.
    *
    * Everyone in front of k will either buy k or less than k. Everyone after k will buy on
    * tickets(k) - 1 tickets as the counting stops after k == 0.
    */
  def timeRequiredToBuy(tickets: Array[Int], k: Int): Int =
    (0 until tickets.length).foldLeft(0): (acc, index) =>
      if index <= k then tickets(index).min(tickets(k)) + acc
      else tickets(index).min(tickets(k) - 1) + acc

    /** 463. Island perimeter.
      *
      * You are given row x col grid representing a map where grid[i][j] = 1 represents land and
      * grid[i][j] = 0 represents water.
      *
      * Grid cells are connected horizontally/vertically (not diagonally). The grid is completely
      * surrounded by water, and there is exactly one island (i.e., one or more connected land
      * cells).
      *
      * The island doesn't have "lakes", meaning the water inside isn't connected to the water
      * around the island. One cell is a square with side length 1. The grid is rectangular, width
      * and height don't exceed 100. Determine the perimeter of the island.
      *
      * ```
      * Example 1:
      *
      * - Input: grid = [[0,1,0,0],[1,1,1,0],[0,1,0,0],[1,1,0,0]]
      * - Output: 16
      * - Explanation: The perimeter is the 16 yellow stripes in the image above.
      *
      * Example 2:
      *
      * - Input: grid = [[1]]
      * - Output: 4
      *
      * Example 3:
      *
      * - Input: grid = [[1,0]]
      * - Output: 4
      * ```
      * Constraints:
      *   - row == grid.length
      *   - col == grid[i].length
      *   - 1 <= row, col <= 100
      *   - grid[i][j] is 0 or 1.
      *   - There is exactly one island in grid.
      *
      * @param Array[Array[Int]]
      * @return
      *   Int
      */
  def islandPerimeter(grid: Array[Array[Int]]): Int =

    val rRange = 0 until grid.length
    val cRange = 0 until grid(0).length

    /* pretty print given grid with a comment (debugging purpose)*/
    def pprint(grid: Array[Array[Int]], comment: Option[String] = None): Unit =
      val values = (for r <- rRange
      yield (for c <- cRange
      yield s"${grid(r)(c)}").toList).toList
      comment match
        case None => println("Matrix !!")
        case Some(string) =>
          println(string)
          println("*" * string.length())
      values.foreach: list =>
        print(list.mkString(" "))
        println

    /* given r,c - returns number of neighbouring islands */
    def neighboursFor(row: Int, col: Int): Int =
      val neighbours = List((row, col - 1), (row, col + 1), (row - 1, col), (row + 1, col))
      val present = neighbours
        .filter: (x, y) =>
          rRange.contains(x) && cRange.contains(y)
        .map(point => grid(point._1)(point._2))
        .sum
      present

    // pprint(grid = grid, Option("Given input"))
    val ps =
      for r <- rRange
      yield for c <- cRange
      yield
        if grid(r)(c) == 0 then 0
        else 4 - neighboursFor(r, c)
    ps.map(_.sum).sum

  /** 1431. Kids with greatest number of candies.
    *
    * There are n kids with candies. You are given an integer array candies, where each candies[i]
    * represents the number of candies the ith kid has, and an integer extraCandies, denoting the
    * number of extra candies that you have.
    *
    * Return a boolean array result of length n, where result[i] is true if, after giving the ith
    * kid all the extraCandies, they will have the greatest number of candies among all the kids, or
    * false otherwise.
    *
    * Note that multiple kids can have the greatest number of candies.
    *
    * Example 1:
    *
    * Input: candies = [2,3,5,1,3], extraCandies = 3 Output: [true,true,true,false,true]
    * Explanation: If you give all extraCandies to:
    *   - Kid 1, they will have 2 + 3 = 5 candies, * which is the greatest among the kids.
    *   - Kid 2, they will have 3 + 3 = 6 candies, which is the greatest among the kids.
    *   - Kid 3, they will have 5 + 3 = 8 candies, which is the greatest among the kids.
    *   - Kid 4, they will have 1 + 3 = 4 candies, which is not the greatest among the kids.
    *   - Kid 5, they will have 3 + 3 = 6 candies, which is the greatest among the kids.
    *
    * Example 2:
    *
    *   - Input: candies = [4,2,1,1,2], extraCandies = 1
    *   - Output: [true,false,false,false,false]
    *   - Explanation: There is only 1 extra candy. Kid 1 will always have the greatest number of
    *     candies, even if a different kid is given the extra candy.
    *
    * Example 3:
    *
    *   - Input: candies = [12,1,12], extraCandies = 10
    *   - Output: [true,false,true]
    *
    * Constraints:
    *
    *   - n == candies.length
    *   - 2 <= n <= 100
    *   - 1 <= candies[i] <= 100
    *   - 1 <= extraCandies <= 50
    */

  def kidsWithCandies(candies: Array[Int], extraCandies: Int): List[Boolean] =
    val max = candies.max
    candies.map(_ + extraCandies >= max).toList

  /** 605. Can place flowers
    *
    * You have a long flowerbed in which some of the plots are planted, and some are not. However,
    * flowers cannot be planted in adjacent plots.
    *
    * Given an integer array flowerbed containing 0's and 1's, where 0 means empty and 1 means not
    * empty, and an integer n, return true if n new flowers can be planted in the flowerbed without
    * violating the no-adjacent-flowers rule and false otherwise.
    *
    * Example 1:
    *
    *   - Input: flowerbed = [1,0,0,0,1], n = 1
    *   - Output: true
    *
    * Example 2:
    *
    *   - Input: flowerbed = [1,0,0,0,1], n = 2
    *   - Output: false
    *
    * Constraints:
    *
    *   - 1 <= flowerbed.length <= 2 * 10^4
    *   - flowerbed[i] is 0 or 1. There are no two adjacent flowers in * flowerbed. 0 <= n <=
    *     flowerbed.length
    *
    * ##### Approach
    *
    *   - If the bed is List(0), 1 plant can be planted. This is true because of the assumption
    *     List(0) => [0]-List(0)-[0] => [0,0,0]
    *   - Going with this assumption, add head and tail to flowerbed with value = 0.
    *   - iterate from 1 until bed.length -1 - skip the first and last because of assumption.
    *   - now start matching if pointer == 0 and look for prev == next == 0.
    */
  def canPlaceFlowers(flowerbed: Array[Int], n: Int): Boolean =
    @scala.annotation.tailrec
    def iterate(bed: Array[Int], current: Int, terminal: Int, planted: Int): Boolean =
      if current <= terminal && planted < n then
        (bed(current - 1), bed(current), bed(current + 1)) match
          case (0, 0, 0) =>
            bed(current) = 1
            iterate(bed, current + 1, terminal, planted + 1)
          case _ =>
            iterate(bed, current + 1, terminal, planted)
      else planted == n
    iterate(0 +: flowerbed :+ 0, current = 1, terminal = flowerbed.length, 0)

  /** 238. Product of array except self.
    *
    * Given an integer array nums, return an array answer such that answer[i] is equal to the
    * product of all the elements of nums except nums[i].
    *
    * The product of any prefix or suffix of nums is guaranteed to fit in a 32-bit integer.
    *
    * You must write an algorithm that runs in O(n) time and without using the division operation.
    *
    * Example 1:
    *
    *   - Input: nums = [1,2,3,4]
    *   - Output: [24,12,8,6]
    *
    * Example 2:
    *
    *   - Input: nums = [-1,1,0,-3,3]
    *   - Output: [0,0,9,0,0]
    *
    * Constraints:
    *
    *   - 2 <= nums.length <= 10^5
    *   - -30 <= nums[i] <= 30
    *   - The product of any prefix or suffix of nums is guaranteed to fit in a 32-bit integer.
    *
    * ##### Approach
    *
    * Scan will include the extra starting 1 so there are 5 elements not a problem for prefix, but
    * it causes the indices to be off by 1 for suffixes, so I take the tail
    */
  def productExceptSelf(nums: Array[Int]): Array[Int] =
    val prefixProducts: Array[Int] = nums.scanLeft(1)(_ * _)
    val suffixProducts: Array[Int] = nums.scanRight(1)(_ * _).tail
    nums.indices
      .map(i => prefixProducts(i) * suffixProducts(i))
      .toArray

  /** 283. Move zeroes.
    *
    * Given an integer array nums, move all 0's to the end of it while maintaining the relative
    * order of the non-zero elements.
    *
    * Note that you must do this in-place without making a copy of the array.
    *
    * Example 1:
    *
    *   - Input: nums = [0,1,0,3,12]
    *   - Output: [1,3,12,0,0]
    *
    * Example 2:
    *
    *   - Input: nums = [0]
    *   - Output: [0]
    *
    * Constraints:
    *
    *   - 1 <= nums.length <= 10^4
    *   - -2^31 <= nums[i] <= 2^31 - 1
    *
    * Follow up: Could you minimize the total number of operations done?
    */
  def moveZeroes(nums: Array[Int]): Unit =
    @scala.annotation.tailrec
    def loop(first: Int, second: Int): Unit =
      if second < nums.length then
        (nums(first), nums(second)) match
          case (x @ 0, y @ 0) => loop(first, second + 1)
          case (x @ 0, y) if y != 0 =>
            nums(first) = nums(second)
            nums(second) = 0
            // println(s"[${nums.mkString(",")}]")
            loop(first + 1, second + 1)
          case (x, y) if x != 0 && y != 0 => loop(first + 1, second + 1)
    loop(0, 0)

  /** 66. Plus one.
    *
    * You are given a large integer represented as an integer array digits, where each digits[i] is
    * the ith digit of the integer. The digits are ordered from most significant to least
    * significant in left-to-right order. The large integer does not contain any leading 0's.
    *
    * Increment the large integer by one and return the resulting array of digits.
    *
    * Example 1:
    *
    *   - Input: digits = [1,2,3]
    *   - Output: [1,2,4]
    *   - Explanation: The array represents the integer 123. Incrementing by one gives 123 + 1 =
    *     124. Thus, the result should be [1,2,4].
    *
    * Example 2:
    *
    *   - Input: digits = [4,3,2,1]
    *   - Output: [4,3,2,2]
    *   - Explanation: The array represents the integer 4321. Incrementing by one gives 4321 + 1 =
    *     4322. Thus, the result should be [4,3,2,2].
    *
    * Example 3:
    *
    *   - Input: digits = [9]
    *   - Output: [1,0]
    *   - Explanation: The array represents the integer 9. Incrementing by one gives 9 + 1 = 10.
    *     Thus, the result should be [1,0].
    *
    * Constraints:
    *
    *   - 1 <= digits.length <= 100
    *   - 0 <= digits[i] <= 9
    *   - digits does not contain any leading 0's.
    */
  def plusOne(digits: Array[Int]): Array[Int] =
    @scala.annotation.tailrec
    def loop(index: Int, carry: Int, acc: Array[Int]): Array[Int] =
      if index >= 0 then
        val sum = carry + digits(index)
        val q = sum / 10
        val r = sum % 10
        loop(index - 1, q, acc :+ r)
      else if carry > 0 then acc :+ carry
      else acc
    loop(digits.length - 1, 1, Array.emptyIntArray).reverse

  /** 1822. Sign of the product of an array.
    *
    * There is a function signFunc(x) that returns:
    *
    *   - 1 if x is positive.
    *   - -1 if x is negative.
    *   - 0 if x is equal to 0.
    *
    * You are given an integer array nums. Let product be the product of all values in the array
    * nums.
    *
    * Return signFunc(product).
    *
    * Example 1:
    *
    *   - Input: nums = [-1,-2,-3,-4,3,2,1]
    *   - Output: 1
    *   - Explanation: The product of all values in the array is 144, and signFunc(144) = 1
    *
    * Example 2:
    *
    *   - Input: nums = [1,5,0,2,-3]
    *   - Output: 0
    *   - Explanation: The product of all values in the array is 0, and signFunc(0) = 0
    *
    * Example 3:
    *
    *   - Input: nums = [-1,1,-1,1,-1]
    *   - Output: -1
    *   - Explanation: The product of all values in the array is -1, and signFunc(-1) = -1
    *
    * Constraints:
    *
    *   - 1 <= nums.length <= 1000
    *   - -100 <= nums[i] <= 100
    *
    * @param integers
    * @return
    *   - -1 when product < 0
    *   - 0 when product == 0
    *   - 1 when product > 0
    *
    * @note
    *   programming-skils
    */
  def arraySign(nums: Array[Int]): Int =
    @scala.annotation.tailrec
    def loop(index: Int, acc: Int): Int =
      if index == nums.length then acc
      else if nums(index) == 0 then 0
      else loop(index + 1, acc * (if nums(index) > 0 then 1 else -1))
    loop(0, 1)

  /** 1502. Can make arithmetic progression from sequence.
    *
    * A sequence of numbers is called an arithmetic progression if the difference between any two
    * consecutive elements is the same.
    *
    * Given an array of numbers arr, return true if the array can be rearranged to form an
    * arithmetic progression. Otherwise, return false.
    *
    * Example 1:
    *
    *   - Input: arr = [3,5,1]
    *   - Output: true
    *   - Explanation: We can reorder the elements as [1,3,5] or [5,3,1] with differences 2 and -2
    *     respectively, between each consecutive elements.
    *
    * Example 2:
    *
    *   - Input: arr = [1,2,4]
    *   - Output: false
    *   - Explanation: There is no way to reorder the elements to obtain an arithmetic progression.
    *
    * Constraints:
    *
    *   - 2 <= arr.length <= 1000
    *   - -10^6 <= arr[i] <= 10^6
    *
    * @param integers
    * @return
    *   true when AP can be formed from sequence
    *
    * @note
    *   programming-skills
    */
  def canMakeArithmeticProgression(arr: Array[Int]): Boolean =
    arr.sorted
      .sliding(2)
      .toList
      .map(pairAsList => pairAsList(1) - pairAsList(0))
      .distinct
      .size == 1

  /** 896. Monotonic array.
    *
    * An array is monotonic if it is either monotone increasing or monotone decreasing.
    *
    * An array nums is monotone increasing if for all i <= j, nums[i] <= nums[j]. An array nums is
    * monotone decreasing if for all i <= j, nums[i] >= nums[j].
    *
    * Given an integer array nums, return true if the given array is monotonic, or false otherwise.
    *
    * Example 1:
    *
    *   - Input: nums = [1,2,2,3]
    *   - Output: true
    *
    * Example 2:
    *
    *   - Input: nums = [6,5,4,4]
    *   - Output: true
    *
    * Example 3:
    *
    *   - Input: nums = [1,3,2]
    *   - Output: false
    *
    * Constraints:
    *
    *   - 1 <= nums.length <= 10^5
    *   - -10^5 <= nums[i] <= 10^5
    */
  def isMonotonic(nums: Array[Int]): Boolean =
    var isInc = false
    var isDec = false
    for i <- 1 until nums.length do
      if nums(i) > nums(i - 1) then isInc = true
      else if nums(i) < nums(i - 1) then isDec = true

    if isInc && isDec then false
    else true

  /** 682. Baseball game
    *
    * You are keeping the scores for a baseball game with strange rules. At the beginning of the game, you start with an empty record.
    *
    * You are given a list of strings operations, where operations[i] is the ith operation you must apply to the record and is one of the following:
    *
    *    An integer x.
    *        Record a new score of x.
    *    '+'.
    *        Record a new score that is the sum of the previous two scores.
    *    'D'.
    *        Record a new score that is the double of the previous score.
    *    'C'.
    *        Invalidate the previous score, removing it from the record.
    *
    * Return the sum of all the scores on the record after applying all the operations.
    *
    * The test cases are generated such that the answer and all intermediate calculations fit in a 32-bit integer and that all operations are valid.
    *
    * Example 1:
    *
    * - Input: ops = ["5","2","C","D","+"]
    * - Output: 30
    * - Explanation:
    *     - "5" - Add 5 to the record, record is now [5].
    *     - "2" - Add 2 to the record, record is now [5, 2].
    *     - "C" - Invalidate and remove the previous score, record is now [5].
    *     - "D" - Add 2 * 5 = 10 to the record, record is now [5, 10].
    *     - "+" - Add 5 + 10 = 15 to the record, record is now [5, 10, 15].
    *     The total sum is 5 + 10 + 15 = 30.
    *
    * Example 2:
    *
    * - Input: ops = ["5","-2","4","C","D","9","+","+"]
    * - Output: 27
    * - Explanation:
    *   - "5" - Add 5 to the record, record is now [5].
    *   - "-2" - Add -2 to the record, record is now [5, -2].
    *   - "4" - Add 4 to the record, record is now [5, -2, 4].
    *   - "C" - Invalidate and remove the previous score, record is now [5, -2].
    *   - "D" - Add 2 * -2 = -4 to the record, record is now [5, -2, -4].
    *   - "9" - Add 9 to the record, record is now [5, -2, -4, 9].
    *   - "+" - Add -4 + 9 = 5 to the record, record is now [5, -2, -4, 9, 5].
    *   - "+" - Add 9 + 5 = 14 to the record, record is now [5, -2, -4, 9, 5, 14].
    *   The total sum is 5 + -2 + -4 + 9 + 5 + 14 = 27.
    *
    * Example 3:
    *
    * - Input: ops = ["1","C"]
    * - Output: 0
    * - Explanation:
    *   - "1" - Add 1 to the record, record is now [1].
    *   - "C" - Invalidate and remove the previous score, record is now [].
    *   Since the record is empty, the total sum is 0.
    *
    * Constraints:
    *
    *   - 1 <= operations.length <= 1000
    *   - operations[i] is "C", "D", "+", or a string representing an integer in the range [-3 * 10^4, 3 * 10^4].
    *   - For operation "+", there will always be at least two previous scores on the record.
    *   - For operations "C" and "D", there will always be at least one previous score on the record.
    */

  def calPoints(operations: Array[String]): Int =
    val scores =
      operations
        .foldLeft(List.empty[Int]): (acc, element) =>
          element match
            case "C" => acc.tail
            case "D" => (acc.head * 2) +: acc
            case "+" => (acc.head + acc.tail.head) +: acc
            case n   => n.toInt +: acc
    // println(s"operations: [${operations.mkString(",")}] => [${scores.mkString(",")}]")
    scores.sum

  /** 1275. Find winner on a tic tac toe game.
    *
    * Tic-tac-toe is played by two players A and B on a 3 x 3 grid. The rules of Tic-Tac-Toe are:
    *
    *    Players take turns placing characters into empty squares ' '.
    *    The first player A always places 'X' characters, while the second player B always places 'O' characters.
    *    'X' and 'O' characters are always placed into empty squares, never on filled ones.
    *    The game ends when there are three of the same (non-empty) character filling any row, column, or diagonal.
    *    The game also ends if all squares are non-empty.
    *    No more moves can be played if the game is over.
    *
    * Given a 2D integer array moves where moves[i] = [rowi, coli] indicates that the ith move will be played on grid[rowi][coli]. return the winner of the game if it exists (A or B). In case the game ends in a draw return "Draw". If there are still movements to play return "Pending".
    *
    * You can assume that moves is valid (i.e., it follows the rules of Tic-Tac-Toe), the grid is initially empty, and A will play first.
    *
    * Example 1
    *
    * | X |   |   |
    * |   | X |   |
    * | 0 | 0 | X |
    * - Input: moves = [[0,0],[2,0],[1,1],[2,1],[2,2]]
    * - Output: "A"
    * - Explanation: A wins, they always play first.
    *
    * ### Approach
    * - There are 8 lines to be considered; 3 row, 3 col and 2 diagonals.
    * - Place the moves and parse to find the player where one of the lines is completely occupied.
    */
  def tictactoe(moves: Array[Array[Int]]): String =
    val arr = Array.ofDim[String](3, 3) // O(n*n)
    val played = moves.length
    // Fill board O(n)
    for i <- 0 until played by 2
    do
      val r = moves(i)(0)
      val c = moves(i)(1)
      arr(r)(c) = "A"

    for i <- 1 until played by 2
    do
      val r = moves(i)(0)
      val c = moves(i)(1)
      arr(r)(c) = "B"

    def check: Option[String] =
      var winner: Option[String] = None
      // check row & col O(n)
      for i <- 0 to 2 do {
        if winner.isEmpty && arr(i)(0) == arr(i)(1) && arr(i)(1) == arr(i)(2) && arr(i)(2) != null
        then winner = Some(arr(i)(0))
        if winner.isEmpty && arr(0)(i) == arr(1)(i) && arr(1)(i) == arr(2)(i) && arr(2)(i) != null
        then winner = Some(arr(0)(i))
      }
      // check diagonal  O(1)
      if winner.isEmpty && arr(0)(0) == arr(1)(1) && arr(1)(1) == arr(2)(2) && arr(1)(1) != null
      then winner = Some(arr(1)(1))
      if winner.isEmpty && arr(0)(2) == arr(1)(1) && arr(1)(1) == arr(2)(0) && arr(1)(1) != null
      then winner = Some(arr(1)(1))

      winner

    check match
      case Some(v)             => v
      case None if played == 9 => "Draw"
      case _                   => "Pending"

  /** 2441. Largest positive number that exists with its negative.
    *
    * Given an integer array nums that does not contain any zeros, find the largest positive integer k such that -k also exists in the array.
    *
    * Return the positive integer k. If there is no such integer, return -1.
    *
    * Example 1:
    *
    * - Input: nums = [-1,2,-3,3]
    * - Output: 3
    * - Explanation: 3 is the only valid k we can find in the array.
    *
    * Example 2:
    *
    * - Input: nums = [-1,10,6,7,-7,1]
    * - Output: 7
    * - Explanation: Both 1 and 7 have their corresponding negative values in the array. 7 has a larger value.
    *
    * Example 3:
    *
    * - Input: nums = [-10,8,6,7,-2,-3]
    * - Output: -1
    * - Explanation: There is no a single valid k, we return -1.
    *
    * Constraints:
    *
    *    - 1 <= nums.length <= 1000
    *    - -1000 <= nums[i] <= 1000
    *    - nums[i] != 0
    */
  def findMaxK(nums: Array[Int]): Int =

    val dict = nums.toSet
    val pairs = nums.groupBy(n => dict.contains(n * -1)).getOrElse(true, Array.empty[Int])
    if pairs.length == 0 then -1 else pairs.max

    // val x = nums.groupBy(n => dict.contains(n * -1))
    // @scala.annotation.tailrec
    // def loop(nums: Array[Int], state: Set[Int], largest: Int): Int =
    //   // println(s"[${nums.mkString(",")}] && state:[${state.mkString(",")}] && largest: $largest")
    //   nums.headOption match
    //     case None => largest
    //     case Some(value) =>
    //       if state.contains(value * -1) then
    //         loop(nums.tail, state + value, largest.max(math.abs(value)))
    //       else loop(nums.tail, state + value, largest)
    // // println(s"given [${nums.mkString(",")}]")
    // loop(nums, Set.empty, -1)

  /** 1672. Richest customer wealth.
    *
    * You are given an m x n integer grid accounts where accounts[i][j] is the amount of money the i​​​​​​​​​​​th​​​​ customer has in the j​​​​​​​​​​​th​​​​ bank. Return the wealth that the richest customer has.
    *
    * A customer's wealth is the amount of money they have in all their bank accounts. The richest customer is the customer that has the maximum wealth.
    *
    * Example 1:
    *
    * Input: accounts = [[1,2,3],[3,2,1]]
    * Output: 6
    * Explanation:
    * 1st customer has wealth = 1 + 2 + 3 = 6
    * 2nd customer has wealth = 3 + 2 + 1 = 6
    * Both customers are considered the richest with a wealth of 6 each, so return 6.
    *
    * Example 2:
    *
    * Input: accounts = [[1,5],[7,3],[3,5]]
    * Output: 10
    * Explanation:
    * 1st customer has wealth = 6
    * 2nd customer has wealth = 10
    * 3rd customer has wealth = 8
    * The 2nd customer is the richest with a wealth of 10.
    *
    * Example 3:
    *
    * Input: accounts = [[2,8,7],[7,1,3],[1,9,5]]
    * Output: 17
    *
    * Constraints:
    *
    *    m == accounts.length
    *    n == accounts[i].length
    *    1 <= m, n <= 50
    *    1 <= accounts[i][j] <= 100
    */
  def maximumWealth(accounts: Array[Array[Int]]): Int =
    accounts.map(w => w.sum).max

  /** 165. Compare version numbers.
    *
    * Given two version numbers, version1 and version2, compare them.
    *
    * Version numbers consist of one or more revisions joined by a dot '.'. Each revision consists of digits and may contain leading zeros. Every revision contains at least one character. Revisions are 0-indexed from left to right, with the leftmost revision being revision 0, the next revision being revision 1, and so on. For example 2.5.33 and 0.1 are valid version numbers.
    *
    * To compare version numbers, compare their revisions in left-to-right order. Revisions are compared using their integer value ignoring any leading zeros. This means that revisions 1 and 001 are considered equal. If a version number does not specify a revision at an index, then treat the revision as 0. For example, version 1.0 is less than version 1.1 because their revision 0s are the same, but their revision 1s are 0 and 1 respectively, and 0 < 1.
    *
    * Return the following:
    *
    *    If version1 < version2, return -1.
    *    If version1 > version2, return 1.
    *    Otherwise, return 0.
    *
    * Example 1:
    *
    * - Input: version1 = "1.01", version2 = "1.001"
    * - Output: 0
    * - Explanation: Ignoring leading zeroes, both "01" and "001" represent the same integer "1".
    *
    * Example 2:
    *
    * - Input: version1 = "1.0", version2 = "1.0.0"
    * - Output: 0
    * - Explanation: version1 does not specify revision 2, which means it is treated as "0".
    *
    * Example 3:
    *
    * - Input: version1 = "0.1", version2 = "1.1"
    * - Output: -1
    * - Explanation: version1's revision 0 is "0", while version2's revision 0 is "1". 0 < 1, so version1 < version2.
    *
    * Constraints:
    *    - 1 <= version1.length, version2.length <= 500
    *    - version1 and version2 only contain digits and '.'.
    *    - version1 and version2 are valid version numbers.
    *    - All the given revisions in version1 and version2 can be stored in a 32-bit integer.
    */
  def compareVersion(version1: String, version2: String): Int =
    val ars = version1.split("\\.").map(_.toInt)
    val brs = version2.split("\\.").map(_.toInt)
    val rs = ars.zipAll(brs, 0, 0)
    def loop(pair: Array[(Int, Int)]): Int =
      pair.headOption match
        case (Some(x, y)) if x > y => +1
        case (Some(x, y)) if x < y => -1
        case (Some(x, y))          => loop(pair.tail)
        case None                  => 0
    loop(rs)

  /** 1523. Count of odd numbers in an interval range.
    *
    *  Given two non-negative integers low and high. Return the count of odd numbers between low and high (inclusive).
    *
    * Example 1:
    *
    * - Input: low = 3, high = 7
    * - Output: 3
    * - Explanation: The odd numbers between 3 and 7 are [3,5,7].
    *
    * Example 2:
    *
    * - Input: low = 8, high = 10
    * - Output: 1
    * - Explanation: The odd numbers between 8 and 10 are [9].
    *
    * Constraints:
    *  - 0 <= low <= high <= 10^9
    *
    * ### Approach
    * - Only when high is odd, increment by 1.
    */
  def countOdds(low: Int, high: Int): Int =
    (high / 2) - (low / 2) + high % 2

  /** 1491. Average salary excluding the minimum and maximum salary.
    *
    * You are given an array of unique integers salary where salary[i] is the salary of the ith employee.
    *
    * Return the average salary of employees excluding the minimum and maximum salary. Answers within 10^-5 of the actual answer will be accepted.
    *
    * Example 1:
    *
    * - Input: salary = [4000,3000,1000,2000]
    * - Output: 2500.00000
    * - Explanation: Minimum salary and maximum salary are 1000 and 4000 respectively. Average salary excluding minimum and maximum salary is (2000+3000) / 2 = 2500
    *
    * Example 2:
    *
    * - Input: salary = [1000,2000,3000]
    * - Output: 2000.00000
    * - Explanation: Minimum salary and maximum salary are 1000 and 3000 respectively. Average salary excluding minimum and maximum salary is (2000) / 1 = 2000
    *
    * Constraints:
    *
    * - 3 <= salary.length <= 100
    * - 1000 <= salary[i] <= 106
    * - All the integers of salary are unique.
    */
  def average(salary: Array[Int]): Double =
    val (min, max, sum) =
      salary.tail.foldLeft((salary.head, salary.head, salary.head)): (state, e) =>
        (state, e) match
          case ((min, max, sum), e) => (e.min(min), e.max(max), e + sum)
    // println(s"min: $min, max: $max, sum: $sum, length: ${salary.length}")
    (sum - min - max) / (salary.length - 2).toDouble

  /** 881. Boats to save people.
    *
    * You are given an array people where people[i] is the weight of the ith person, and an infinite number of boats where each boat can carry a maximum weight of limit. Each boat carries at most two people at the same time, provided the sum of the weight of those people is at most limit.
    *
    * Return the minimum number of boats to carry every given person.
    *
    * Example 1:
    *
    * - Input: people = [1,2], limit = 3
    * - Output: 1
    * - Explanation: 1 boat (1, 2)
    *
    * Example 2:
    *
    * - Input: people = [3,2,2,1], limit = 3
    * - Output: 3
    * - Explanation: 3 boats (1, 2), (2) and (3)
    *
    * Example 3:
    *
    * - Input: people = [3,5,3,4], limit = 5
    * - Output: 4
    * - Explanation: 4 boats (3), (3), (4), (5)
    *
    * Constraints:
    *  - 1 <= people.length <= 5 * 10^4
    *  - 1 <= people[i] <= limit <= 3 * 10^4
    */

  def numRescueBoats(people: Array[Int], limit: Int): Int =
    val sorted = people.sorted

    // approach # 1 (just with trip counter)
    // @scala.annotation.tailrec
    // def loop(l: Int, r: Int, acc: Int): Int =
    //   println(s"l: $l, r: $r, acc: $acc")
    //   if r > l then
    //     (sorted(l), sorted(r)) match
    //       case (x, y) if x + y <= limit => loop(l + 1, r - 1, acc + 1) // within limit, accept both
    //       case (x, y) => loop(l, r - 1, acc + 1) // beyond limit, accept only heavier of the two
    //   else if r == l then acc + 1
    //   else acc

    // println(s"given: [${people.mkString(",")}], sorted: [${sorted.mkString(",")}]")
    // loop(0, people.length - 1, 0)

    // approach # 2 (with grouping of people per boat. )
    @scala.annotation.tailrec
    def loop(l: Int, r: Int, boats: List[List[Int]]): Int =
      if r > l then
        (sorted(l), sorted(r)) match
          case (x, y) if x + y <= limit => loop(l + 1, r - 1, List(x, y) +: boats)
          case (x, y)                   => loop(l, r - 1, List(List(y)) ++ boats)
      else if r == l then loop(l + 1, r - 1, List(List(sorted(r))) ++ boats)
      else boats.size
    loop(0, sorted.length - 1, List.empty)

  /** 2785. Sort vowel in a string.
    *
    * Given a 0-indexed string s, permute s to get a new string t such that:
    *
    *    All consonants remain in their original places. More formally, if there is an index i with 0 <= i < s.length such that s[i] is a consonant, then t[i] = s[i].
    *    The vowels must be sorted in the nondecreasing order of their ASCII values. More formally, for pairs of indices i, j with 0 <= i < j < s.length such that s[i] and s[j] are vowels, then t[i] must not have a higher ASCII value than t[j].
    *
    * Return the resulting string.
    *
    * The vowels are 'a', 'e', 'i', 'o', and 'u', and they can appear in lowercase or uppercase. Consonants comprise all letters that are not vowels.
    *
    * Example 1:
    *
    * Input: s = "lEetcOde"
    * Output: "lEOtcede"
    * Explanation: 'E', 'O', and 'e' are the vowels in s; 'l', 't', 'c', and 'd' are all consonants. The vowels are sorted according to their ASCII values, and the consonants remain in the same places.
    *
    * Example 2:
    *
    * Input: s = "lYmpH"
    * Output: "lYmpH"
    * Explanation: There are no vowels in s (all characters in s are consonants), so we return "lYmpH".
    *
    * Constraints:
    *
    *    1 <= s.length <= 105
    *    s consists only of letters of the English alphabet in uppercase and lowercase.
    */

  def sortVowels(s: String): String = {
    val vowels = "aeiou".toSet
    val sorted = s.filter(c => vowels.contains(c.toLower)).sorted.mkString
    @scala.annotation.tailrec
    def traverse(i: Int, j: Int, acc: List[Char]): String =
      if i == s.length then acc.reverse.mkString
      else if vowels.contains(s(i).toLower) then traverse(i + 1, j + 1, sorted(j) +: acc)
      else traverse(i + 1, j, s(i) +: acc)
    traverse(0, 0, List.empty[Char])
  }

  /** 1929. Concatenation of array.
    *
    * Given an integer array nums of length n, you want to create an array ans of length 2n where ans[i] == nums[i] and ans[i + n] == nums[i] for 0 <= i < n (0-indexed).
    *
    * Specifically, ans is the concatenation of two nums arrays.
    *
    * Return the array ans.
    *
    * Example 1:
    *
    * Input: nums = [1,2,1]
    * Output: [1,2,1,1,2,1]
    * Explanation: The array ans is formed as follows:
    * - ans = [nums[0],nums[1],nums[2],nums[0],nums[1],nums[2]]
    * - ans = [1,2,1,1,2,1]
    *
    * Example 2:
    *
    * Input: nums = [1,3,2,1]
    * Output: [1,3,2,1,1,3,2,1]
    * Explanation: The array ans is formed as follows:
    * - ans = [nums[0],nums[1],nums[2],nums[3],nums[0],nums[1],nums[2],nums[3]]
    * - ans = [1,3,2,1,1,3,2,1]
    *
    * Constraints:
    *
    *    n == nums.length
    *    1 <= n <= 1000
    *    1 <= nums[i] <= 1000
    */
  def getConcatenation(nums: Array[Int]): Array[Int] = {
    val ans = Array.ofDim[Int](nums.length * 2)
    for i <- 0 until ans.length
    yield ans(i) = nums(i % nums.length)
    ans
  }

  /** 1512. Number of good pairs.
    *
    * Given an array of integers nums, return the number of good pairs.
    *
    * A pair (i, j) is called good if nums[i] == nums[j] and i < j.
    *
    * Example 1:
    *
    * Input: nums = [1,2,3,1,1,3]
    * Output: 4
    * Explanation: There are 4 good pairs (0,3), (0,4), (3,4), (2,5) 0-indexed.
    *
    * Example 2:
    *
    * Input: nums = [1,1,1,1]
    * Output: 6
    * Explanation: Each pair in the array are good.
    *
    * Example 3:
    *
    * Input: nums = [1,2,3]
    * Output: 0
    *
    * Constraints:
    *
    *    1 <= nums.length <= 100
    *    1 <= nums[i] <= 100
    */
  def numIdenticalPairs(nums: Array[Int]): Int = {
    val pairs = for
      i <- 0 until nums.length
      j <- 0 until i
    yield (nums(i), nums(j))
    pairs.count:
      case (x, y) => x == y
  }

  /** 1470. Shuffle the array.
    *
    * Given the array nums consisting of 2n elements in the form [x1,x2,...,xn,y1,y2,...,yn].
    *
    * Return the array in the form [x1,y1,x2,y2,...,xn,yn].
    *
    * Example 1:
    *
    * Input: nums = [2,5,1,3,4,7], n = 3
    * Output: [2,3,5,4,1,7]
    * Explanation: Since x1=2, x2=5, x3=1, y1=3, y2=4, y3=7 then the answer is [2,3,5,4,1,7].
    *
    * Example 2:
    *
    * Input: nums = [1,2,3,4,4,3,2,1], n = 4
    * Output: [1,4,2,3,3,2,4,1]
    *
    * Example 3:
    *
    * Input: nums = [1,1,2,2], n = 2
    * Output: [1,2,1,2]
    *
    * Constraints:
    *
    *    1 <= n <= 500
    *    nums.length == 2n
    *    1 <= nums[i] <= 10^3
    */
  def shuffle(nums: Array[Int], n: Int): Array[Int] = {
    (nums.take(n) zip nums.drop(n)).flatMap(x => List(x._1, x._2))
  }

  /** 1365. How many numbers are smaller than the current numbers.
    *
    * Given the array nums, for each nums[i] find out how many numbers in the array are smaller than it. That is, for each nums[i] you have to count the number of valid j's such that j != i and nums[j] < nums[i].
    *
    * Return the answer in an array.
    *
    * Example 1:
    *
    * Input: nums = [8,1,2,2,3]
    * Output: [4,0,1,1,3]
    * Explanation:
    * For nums[0]=8 there exist four smaller numbers than it (1, 2, 2 and 3).
    * For nums[1]=1 does not exist any smaller number than it.
    * For nums[2]=2 there exist one smaller number than it (1).
    * For nums[3]=2 there exist one smaller number than it (1).
    * For nums[4]=3 there exist three smaller numbers than it (1, 2 and 2).
    *
    * Example 2:
    *
    * Input: nums = [6,5,4,8]
    * Output: [2,1,0,3]
    *
    * Example 3:
    *
    * Input: nums = [7,7,7,7]
    * Output: [0,0,0,0]
    *
    * Constraints:
    *
    *    2 <= nums.length <= 500
    *    0 <= nums[i] <= 100
    */

  def smallerNumbersThanCurrent(nums: Array[Int]): Array[Int] = {
    nums.map(n => nums.count(_ < n))
  }

  /** 506. Relative ranks.
    *
    * ou are given an integer array score of size n, where score[i] is the score of the ith athlete in a competition. All the scores are guaranteed to be unique.
    *
    * The athletes are placed based on their scores, where the 1st place athlete has the highest score, the 2nd place athlete has the 2nd highest score, and so on. The placement of each athlete determines their rank:
    *
    *    The 1st place athlete's rank is "Gold Medal".
    *    The 2nd place athlete's rank is "Silver Medal".
    *    The 3rd place athlete's rank is "Bronze Medal".
    *    For the 4th place to the nth place athlete, their rank is their placement number (i.e., the xth place athlete's rank is "x").
    *
    * Return an array answer of size n where answer[i] is the rank of the ith athlete.
    *
    * Example 1:
    *
    * Input: score = [5,4,3,2,1]
    * Output: ["Gold Medal","Silver Medal","Bronze Medal","4","5"]
    * Explanation: The placements are [1st, 2nd, 3rd, 4th, 5th].
    *
    * Example 2:
    *
    * Input: score = [10,3,8,9,4]
    * Output: ["Gold Medal","5","Bronze Medal","Silver Medal","4"]
    * Explanation: The placements are [1st, 5th, 3rd, 2nd, 4th].
    *
    * Constraints:
    *
    * - n == score.length
    * - 1 <= n <= 10^4
    * - 0 <= score[i] <= 10^6
    * - All the values in score are unique.
    */

  def findRelativeRanks(score: Array[Int]): Array[String] = {
    val sorted = score.sortWith(_ >= _).zipWithIndex
    score.map: s =>
      sorted.find(_._1 == s) match
        case Some((_, 0))     => "Gold Medal"
        case Some((_, 1))     => "Silver Medal"
        case Some((_, 2))     => "Bronze Medal"
        case Some((_, index)) => (index + 1).toString
        case _                => ""
  }

  /** 2798. Number of employees who met the target.
    *
    * There are n employees in a company, numbered from 0 to n - 1. Each employee i has worked for hours[i] hours in the company.
    *
    * The company requires each employee to work for at least target hours.
    *
    * You are given a 0-indexed array of non-negative integers hours of length n and a non-negative integer target.
    *
    * Return the integer denoting the number of employees who worked at least target hours.
    *
    * Example 1:
    *
    * Input: hours = [0,1,2,3,4], target = 2
    * Output: 3
    * Explanation: The company wants each employee to work for at least 2 hours.
    * - Employee 0 worked for 0 hours and didn't meet the target.
    * - Employee 1 worked for 1 hours and didn't meet the target.
    * - Employee 2 worked for 2 hours and met the target.
    * - Employee 3 worked for 3 hours and met the target.
    * - Employee 4 worked for 4 hours and met the target.
    * There are 3 employees who met the target.
    *
    * Example 2:
    *
    * Input: hours = [5,1,4,2,2], target = 6
    * Output: 0
    * Explanation: The company wants each employee to work for at least 6 hours.
    * There are 0 employees who met the target.
    *
    * Constraints:
    *
    *    1 <= n == hours.length <= 50
    *    0 <= hours[i], target <= 105
    */

  def numberOfEmployeesWhoMetTarget(hours: Array[Int], target: Int): Int = hours.count(_ >= target)

  /** 2824. Count pairs whose sum is less than target.
    *
    * Given a 0-indexed integer array nums of length n and an integer target, return the number of pairs (i, j) where 0 <= i < j < n and nums[i] + nums[j] < target.
    *
    * Example 1:
    *
    * Input: nums = [-1,1,2,3,1], target = 2
    * Output: 3
    * Explanation: There are 3 pairs of indices that satisfy the conditions in the statement:
    * - (0, 1) since 0 < 1 and nums[0] + nums[1] = 0 < target
    * - (0, 2) since 0 < 2 and nums[0] + nums[2] = 1 < target
    * - (0, 4) since 0 < 4 and nums[0] + nums[4] = 0 < target
    * Note that (0, 3) is not counted since nums[0] + nums[3] is not strictly less than the target.
    *
    * Example 2:
    *
    * Input: nums = [-6,2,5,-2,-7,-1,3], target = -2
    * Output: 10
    * Explanation: There are 10 pairs of indices that satisfy the conditions in the statement:
    * - (0, 1) since 0 < 1 and nums[0] + nums[1] = -4 < target
    * - (0, 3) since 0 < 3 and nums[0] + nums[3] = -8 < target
    * - (0, 4) since 0 < 4 and nums[0] + nums[4] = -13 < target
    * - (0, 5) since 0 < 5 and nums[0] + nums[5] = -7 < target
    * - (0, 6) since 0 < 6 and nums[0] + nums[6] = -3 < target
    * - (1, 4) since 1 < 4 and nums[1] + nums[4] = -5 < target
    * - (3, 4) since 3 < 4 and nums[3] + nums[4] = -9 < target
    * - (3, 5) since 3 < 5 and nums[3] + nums[5] = -3 < target
    * - (4, 5) since 4 < 5 and nums[4] + nums[5] = -8 < target
    * - (4, 6) since 4 < 6 and nums[4] + nums[6] = -4 < target
    *
    * Constraints:
    *
    *    1 <= nums.length == n <= 50
    *    -50 <= nums[i], target <= 50
    */

  def countPairs(nums: List[Int], target: Int): Int = {
    nums.indices
      .flatMap: i =>
        (0 until i).filter(j => nums(i) + nums(j) < target)
      .size
    // (for
    //   i <- nums.indices
    //   j <- 0 until i
    //   if nums(i) + nums(j) < target
    // yield (i, j)).size
  }

  /** 1480. Running sum of 1-d array.
    *
    * Given an array nums. We define a running sum of an array as runningSum[i] = sum(nums[0]…nums[i]).
    *
    * Return the running sum of nums.
    *
    * Example 1:
    *
    * Input: nums = [1,2,3,4]
    * Output: [1,3,6,10]
    * Explanation: Running sum is obtained as follows: [1, 1+2, 1+2+3, 1+2+3+4].
    *
    * Example 2:
    *
    * Input: nums = [1,1,1,1,1]
    * Output: [1,2,3,4,5]
    * Explanation: Running sum is obtained as follows: [1, 1+1, 1+1+1, 1+1+1+1, 1+1+1+1+1].
    *
    * Example 3:
    *
    * Input: nums = [3,1,2,10,1]
    * Output: [3,4,6,16,17]
    *
    * Constraints:
    *
    *    1 <= nums.length <= 1000
    *    -10^6 <= nums[i] <= 10^6
    */

  def runningSum(nums: Array[Int]): Array[Int] = {
    for i <- 1 until nums.length
    do nums(i) = nums(i - 1) + nums(i)
    nums
    // nums.scanLeft(0)(_ + _).tail
  }

  /** Decompress run-length encoded list.
    *
    * We are given a list nums of integers representing a list compressed with run-length encoding.
    *
    * Consider each adjacent pair of elements [freq, val] = [nums[2*i], nums[2*i+1]] (with i >= 0).  For each such pair, there are freq elements with value val concatenated in a sublist. Concatenate all the sublists from left to right to generate the decompressed list.
    *
    * Return the decompressed list.
    *
    * Example 1:
    *
    * Input: nums = [1,2,3,4]
    * Output: [2,4,4,4]
    * Explanation: The first pair [1,2] means we have freq = 1 and val = 2 so we generate the array [2].
    * The second pair [3,4] means we have freq = 3 and val = 4 so we generate [4,4,4].
    * At the end the concatenation [2] + [4,4,4] is [2,4,4,4].
    *
    * Example 2:
    *
    * Input: nums = [1,1,2,3]
    * Output: [1,3,3]
    *
    * Constraints:
    *
    *    2 <= nums.length <= 100
    *    nums.length % 2 == 0
    *    1 <= nums[i] <= 100
    */

  def decompressRLElist(nums: Array[Int]): Array[Int] = {
    nums
      .grouped(2)
      .map(a => a(0) -> a(1))
      .flatMap: (f, e) =>
        Array.fill(f)(e)
      .toArray
  }

  /** 1389. Create target array in the given order.
    *
    * Given two arrays of integers nums and index. Your task is to create target array under the following rules:
    *
    *    - Initially target array is empty.
    *    - From left to right read nums[i] and index[i], insert at index index[i] the value nums[i] in target array.
    *    - Repeat the previous step until there are no elements to read in nums and index.
    *    - Return the target array.
    *
    * It is guaranteed that the insertion operations will be valid.
    *
    * Example 1:
    *
    * Input: nums = [0,1,2,3,4], index = [0,1,2,2,1]
    * Output: [0,4,1,3,2]
    * Explanation:
    * nums       index     target
    * 0            0        [0]
    * 1            1        [0,1]
    * 2            2        [0,1,2]
    * 3            2        [0,1,3,2]
    * 4            1        [0,4,1,3,2]
    *
    * Example 2:
    *
    * Input: nums = [1,2,3,4,0], index = [0,1,2,3,0]
    * Output: [0,1,2,3,4]
    * Explanation:
    * nums       index     target
    * 1            0        [1]
    * 2            1        [1,2]
    * 3            2        [1,2,3]
    * 4            3        [1,2,3,4]
    * 0            0        [0,1,2,3,4]
    *
    * Example 3:
    *
    * Input: nums = [1], index = [0]
    * Output: [1]
    *
    * Constraints:
    *
    * - 1 <= nums.length, index.length <= 100
    * - nums.length == index.length
    * - 0 <= nums[i] <= 100
    * - 0 <= index[i] <= i
    */
  def createTargetArray(nums: Array[Int], index: Array[Int]): Array[Int] = {
    (0 until nums.length)
      .foldLeft(Array.emptyIntArray): (acc, i) =>
        val (left, right) = acc.splitAt(index(i))
        left ++ Array(nums(i)) ++ right
  }

  /** 3075. Maximize happiness of selected children.
    *
    * You are given an array happiness of length n, and a positive integer k.
    *
    * There are n children standing in a queue, where the ith child has happiness value happiness[i]. You want to select k children from these n children in k turns.
    *
    * In each turn, when you select a child, the happiness value of all the children that have not been selected till now decreases by 1. Note that the happiness value cannot become negative and gets decremented only if it is positive.
    *
    * Return the maximum sum of the happiness values of the selected children you can achieve by selecting k children.
    *
    * Example 1:
    *
    * Input: happiness = [1,2,3], k = 2
    * Output: 4
    * Explanation: We can pick 2 children in the following way:
    * - Pick the child with the happiness value == 3. The happiness value of the remaining children becomes [0,1].
    * - Pick the child with the happiness value == 1. The happiness value of the remaining child becomes [0]. Note that the happiness value cannot become less than 0.
    * The sum of the happiness values of the selected children is 3 + 1 = 4.
    *
    * Example 2:
    *
    * Input: happiness = [1,1,1,1], k = 2
    * Output: 1
    * Explanation: We can pick 2 children in the following way:
    * - Pick any child with the happiness value == 1. The happiness value of the remaining children becomes [0,0,0].
    * - Pick the child with the happiness value == 0. The happiness value of the remaining child becomes [0,0].
    * The sum of the happiness values of the selected children is 1 + 0 = 1.
    *
    * Example 3:
    *
    * Input: happiness = [2,3,4,5], k = 1
    * Output: 5
    * Explanation: We can pick 1 child in the following way:
    * - Pick the child with the happiness value == 5. The happiness value of the remaining children becomes [1,2,3].
    * The sum of the happiness values of the selected children is 5.
    *
    * Constraints:
    *
    *    1 <= n == happiness.length <= 2 * 10^5
    *    1 <= happiness[i] <= 10^8
    *    1 <= k <= n
    */

  def maximumHappinessSum(happiness: Array[Int], k: Int): Long = {
    val sorted = happiness.sorted.takeRight(k).reverse
    var result: Long = 0L
    var count = 0
    for i <- 0 until k
    do
      result = result + 0.max(sorted(i) - count)
      count = count + 1
    result
  }

  /** 2574. Left and right sum differences.
    *
    * Given a 0-indexed integer array nums, find a 0-indexed integer array answer where:
    *
    *    answer.length == nums.length.
    *    answer[i] = |leftSum[i] - rightSum[i]|.
    *
    * Where:
    *
    *    leftSum[i] is the sum of elements to the left of the index i in the array nums. If there is no such element, leftSum[i] = 0.
    *    rightSum[i] is the sum of elements to the right of the index i in the array nums. If there is no such element, rightSum[i] = 0.
    *
    * Return the array answer.
    *
    * Example 1:
    *
    * Input: nums = [10,4,8,3]
    * Output: [15,1,11,22]
    * Explanation: The array leftSum is [0,10,14,22] and the array rightSum is [15,11,3,0].
    * The array answer is [|0 - 15|,|10 - 11|,|14 - 3|,|22 - 0|] = [15,1,11,22].
    *
    * Example 2:
    *
    * Input: nums = [1]
    * Output: [0]
    * Explanation: The array leftSum is [0] and the array rightSum is [0].
    * The array answer is [|0 - 0|] = [0].
    *
    * Constraints:
    *
    *    1 <= nums.length <= 1000
    *    1 <= nums[i] <= 10^5
    */
  def leftRightDifference(nums: Array[Int]): Array[Int] = {
    val left = nums.scanLeft(0)(_ + _).take(nums.length)
    val right = nums.scanRight(0)(_ + _).tail
    (for i <- nums.indices yield math.abs(left(i) - right(i))).toArray
  }

  /** 1732. Find the highest altitude
    *
    * There is a biker going on a road trip. The road trip consists of n + 1 points at different altitudes. The biker starts his trip on point 0 with altitude equal 0.
    *
    * You are given an integer array gain of length n where gain[i] is the net gain in altitude between points i​​​​​​ and i + 1 for all (0 <= i < n). Return the highest altitude of a point.
    *
    * Example 1:
    *
    * Input: gain = [-5,1,5,0,-7]
    * Output: 1
    * Explanation: The altitudes are [0,-5,-4,1,1,-6]. The highest is 1.
    *
    * Example 2:
    *
    * Input: gain = [-4,-3,-2,-1,4,3,2]
    * Output: 0
    * Explanation: The altitudes are [0,-4,-7,-9,-10,-6,-3,-1]. The highest is 0.
    *
    * Constraints:
    *
    *    n == gain.length
    *    1 <= n <= 100
    *    -100 <= gain[i] <= 100
    */
  def largestAltitude(gain: Array[Int]): Int = {
    gain.scanLeft(0)(_ + _).max
  }

  /** 3065. Minimum number of operations to exceed the threshold value.
    *
    * You are given a 0-indexed integer array nums, and an integer k.
    *
    * In one operation, you can remove one occurrence of the smallest element of nums.
    *
    * Return the minimum number of operations needed so that all elements of the array are greater than or equal to k.
    *
    * Example 1:
    *
    * Input: nums = [2,11,10,1,3], k = 10
    * Output: 3
    * Explanation: After one operation, nums becomes equal to [2, 11, 10, 3].
    * After two operations, nums becomes equal to [11, 10, 3].
    * After three operations, nums becomes equal to [11, 10].
    * At this stage, all the elements of nums are greater than or equal to 10 so we can stop.
    * It can be shown that 3 is the minimum number of operations needed so that all elements of the array are greater than or equal to 10.
    *
    * Constraints:
    *
    *    1 <= nums.length <= 50
    *    1 <= nums[i] <= 109
    *    1 <= k <= 109
    *    The input is generated such that there is at least one index i such that nums[i] >= k.
    */
  def minOperations(nums: Array[Int], k: Int): Int = {
    nums.count(_ < k)
  }

  /** 169. Majority element.
    *
    * Given an array nums of size n, return the majority element.
    *
    * The majority element is the element that appears more than ⌊n / 2⌋ times. You may assume that the majority element always exists in the array.
    *
    * Example 1:
    *
    * Input: nums = [3,2,3]
    * Output: 3
    *
    * Example 2:
    *
    * Input: nums = [2,2,1,1,1,2,2]
    * Output: 2
    *
    * Constraints:
    *
    *    n == nums.length
    *    1 <= n <= 5 * 10^4
    *    -109 <= nums[i] <= 10^9
    */

  def majorityElement(nums: Array[Int]): Int = {
    nums
      .groupBy(n => n)
      .find:
        case ((n, collection)) => collection.size > nums.length / 2
    match
      case Some((n -> _)) => n
      case None           => -1
  }

  /** 860. Lemonade change.
    *
    * At a lemonade stand, each lemonade costs $5. Customers are standing in a queue to buy from you and order one at a time (in the order specified by bills). Each customer will only buy one lemonade and pay with either a $5, $10, or $20 bill. You must provide the correct change to each customer so that the net transaction is that the customer pays $5.
    *
    * Note that you do not have any change in hand at first.
    *
    * Given an integer array bills where bills[i] is the bill the ith customer pays, return true if you can provide every customer with the correct change, or false otherwise.
    *
    * Example 1:
    *
    * Input: bills = [5,5,5,10,20]
    * Output: true
    * Explanation:
    * From the first 3 customers, we collect three $5 bills in order.
    * From the fourth customer, we collect a $10 bill and give back a $5.
    * From the fifth customer, we give a $10 bill and a $5 bill.
    * Since all customers got correct change, we output true.
    *
    * Example 2:
    *
    * Input: bills = [5,5,10,10,20]
    * Output: false
    * Explanation:
    * From the first two customers in order, we collect two $5 bills.
    * For the next two customers in order, we collect a $10 bill and give back a $5 bill.
    * For the last customer, we can not give the change of $15 back because we only have two $10 bills.
    * Since not every customer received the correct change, the answer is false.
    *
    * Constraints:
    *
    *    1 <= bills.length <= 10^5
    *    bills[i] is either 5, 10, or 20.
    */
  def lemonadeChange(bills: Array[Int]): Boolean = {

    @annotation.tailrec
    def loop(i: Int, fives: Int, tens: Int, twenties: Int): Boolean =
      (i == bills.size) || (bills(i) match {
        case 5                           => loop(i + 1, fives + 1, tens, twenties)
        case 10 if fives > 0             => loop(i + 1, fives - 1, tens + 1, twenties)
        case 20 if tens > 0 && fives > 0 => loop(i + 1, fives - 1, tens - 1, twenties + 1)
        case 20 if fives > 2             => loop(i + 1, fives - 3, tens, twenties + 1)
        case _                           => false
      })

    loop(i = 0, fives = 0, tens = 0, twenties = 0)
  }

  /** 976. Largest perimeter triangle.
    *
    * Given an integer array nums, return the largest perimeter of a triangle with a non-zero area, formed from three of these lengths. If it is impossible to form any triangle of a non-zero area, return 0.
    *
    * Example 1:
    *
    * Input: nums = [2,1,2]
    * Output: 5
    * Explanation: You can form a triangle with three side lengths: 1, 2, and 2.
    *
    * Example 2:
    *
    * Input: nums = [1,2,1,10]
    * Output: 0
    * Explanation:
    * You cannot use the side lengths 1, 1, and 2 to form a triangle.
    * You cannot use the side lengths 1, 1, and 10 to form a triangle.
    * You cannot use the side lengths 1, 2, and 10 to form a triangle.
    * As we cannot use any three side lengths to form a triangle of non-zero area, we return 0.
    *
    * Constraints:
    *
    *    3 <= nums.length <= 10^4
    *    1 <= nums[i] <= 10^6
    *
    * ### Approach
    * - Triangle can be formed only when Sum of two sides should be larger than the 3rd side.
    *   - a + b > c
    *   - b + c > a
    *    - c + a > b, where a,b,c > 0.
    * - Sort reverse.
    * - Group as 3.
    * - Apply the condition and evaluate.
    */

  def largestPerimeter(nums: Array[Int]): Int = {
    nums.sorted.reverse
      .sliding(3)
      .collectFirst:
        case Array(x, y, z) if x < y + z => x + y + z
      .getOrElse(0)
  }

  /** 1232. Check if it is a straight line.
    *
    * You are given an array coordinates, coordinates[i] = [x, y], where [x, y] represents the coordinate of a point. Check if these points make a straight line in the XY plane.
    *
    * Example: 1
    *
    * Input: coordinates = [[1,2],[2,3],[3,4],[4,5],[5,6],[6,7]]
    * Output: true
    *
    * Example: 2
    *
    * * Input: coordinates = [[1,1],[2,2],[3,4],[4,5],[5,6],[7,7]]
    * Output: false
    *
    * Constraints:
    *
    *    2 <= coordinates.length <= 1000
    *    coordinates[i].length == 2
    *    -10^4 <= coordinates[i][0], coordinates[i][1] <= 10^4
    *    coordinates contains no duplicate point.
    *
    * ### Approach
    *
    * - Every point in the line have the same slope.
    * - y2-y1 = m * (x2 - x1), m is the slope
    * - possible to calculate m at every point. Line will have same m.
    * - possible to run into (x2-x1) as 0. To avoid, do not eval slope.
    * - let (y2-y1) / (x2-x1) = m
    * - for every point this should be true.
    * - this means
    *  => (y2-y1) / (x2-x1) = m = (yn-y1) / (xn-x1)
    *  => (y2-y1)(xn-x1) == (yn-y1)(x2-x1)
    */
  def checkStraightLine(coordinates: Array[Array[Int]]): Boolean = {
    val (x1, y1) = (coordinates.head(0), coordinates.head(1))
    val (x2, y2) = (coordinates.tail.head(0), coordinates.tail.head(1))
    val (dx, dy) = (x2 - x1, y2 - y1)
    coordinates.forall:
      case Array(x, y) =>
        (x - x1) * dy == (y - y1) * dx
  }

  /** 1588.  Sum of all odd length subarrays.
    *
    * Given an array of positive integers arr, return the sum of all possible odd-length subarrays of arr.
    *
    * A subarray is a contiguous subsequence of the array.
    *
    * Example 1:
    *
    * Input: arr = [1,4,2,5,3]
    * Output: 58
    * Explanation: The odd-length subarrays of arr and their sums are:
    * [1] = 1
    * [4] = 4
    * [2] = 2
    * [5] = 5
    * [3] = 3
    * [1,4,2] = 7
    * [4,2,5] = 11
    * [2,5,3] = 10
    * [1,4,2,5,3] = 15
    * If we add all these together we get 1 + 4 + 2 + 5 + 3 + 7 + 11 + 10 + 15 = 58
    *
    * Example 2:
    *
    * Input: arr = [1,2]
    * Output: 3
    * Explanation: There are only 2 subarrays of odd length, [1] and [2]. Their sum is 3.
    *
    * Example 3:
    *
    * Input: arr = [10,11,12]
    * Output: 66
    *
    * Constraints:
    *
    *    1 <= arr.length <= 100
    *    1 <= arr[i] <= 1000
    */
  def sumOddLengthSubarrays(arr: Array[Int]): Int = {
    (1 to arr.length by 2)
      .flatMap(arr.sliding(_, 1))
      .flatten
      .sum
  }

  /** 1534. Count good triplets.
    *
    * Given an array of integers arr, and three integers a, b and c. You need to find the number of good triplets.
    *
    * A triplet (arr[i], arr[j], arr[k]) is good if the following conditions are true:
    *
    *    0 <= i < j < k < arr.length
    *    |arr[i] - arr[j]| <= a
    *    |arr[j] - arr[k]| <= b
    *    |arr[i] - arr[k]| <= c
    *
    * Where |x| denotes the absolute value of x.
    *
    * Return the number of good triplets.
    *
    * Example 1:
    *
    * Input: arr = [3,0,1,1,9,7], a = 7, b = 2, c = 3
    * Output: 4
    * Explanation: There are 4 good triplets: [(3,0,1), (3,0,1), (3,1,1), (0,1,1)].
    *
    * Example 2:
    *
    * Input: arr = [1,1,2,2,3], a = 0, b = 0, c = 1
    * Output: 0
    * Explanation: No triplet satisfies all conditions.
    *
    * Constraints:
    *
    *    3 <= arr.length <= 100
    *    0 <= arr[i] <= 1000
    *    0 <= a, b, c <= 1000
    */
  def countGoodTriplets(arr: Array[Int], a: Int, b: Int, c: Int): Int = {
    val n = arr.length
    (for {
      i <- 0 until n
      j <- i + 1 until n
      k <- j + 1 until n
      if math.abs(arr(i) - arr(j)) <= a &&
        math.abs(arr(j) - arr(k)) <= b &&
        math.abs(arr(i) - arr(k)) <= c
    } yield 1).sum
  }

  /** 2956. Find common elements between two arrays.
    *
    * You are given two 0-indexed integer arrays nums1 and nums2 of sizes n and m, respectively.
    *
    * Consider calculating the following values:
    *
    *    The number of indices i such that 0 <= i < n and nums1[i] occurs at least once in nums2.
    *    The number of indices i such that 0 <= i < m and nums2[i] occurs at least once in nums1.
    *
    * Return an integer array answer of size 2 containing the two values in the above order.
    *
    * Example 1:
    *
    * Input: nums1 = [4,3,2,3,1], nums2 = [2,2,5,2,3,6]
    * Output: [3,4]
    * Explanation: We calculate the values as follows:
    * - The elements at indices 1, 2, and 3 in nums1 occur at least once in nums2. So the first value is 3.
    * - The elements at indices 0, 1, 3, and 4 in nums2 occur at least once in nums1. So the second value is 4.
    *
    * Example 2:
    *
    * Input: nums1 = [3,4,2,3], nums2 = [1,5]
    * Output: [0,0]
    * Explanation: There are no common elements between the two arrays, so the two values will be 0.
    *
    * Constraints:
    *
    *    n == nums1.length
    *    m == nums2.length
    *    1 <= n, m <= 100
    *    1 <= nums1[i], nums2[i] <= 100
    */
  def findIntersectionValues(nums1: Array[Int], nums2: Array[Int]): Array[Int] = {
    Array(
      nums1.count(nums2.contains(_)),
      nums2.count(nums1.contains(_))
    )
  }

  /** 2006. Count the number of pairs with absolute difference k.
    *
    * Given an integer array nums and an integer k, return the number of pairs (i, j) where i < j such that |nums[i] - nums[j]| == k.
    *
    * The value of |x| is defined as:
    *
    *    x if x >= 0.
    *    -x if x < 0.
    *
    * Example 1:
    *
    * Input: nums = [1,2,2,1], k = 1
    * Output: 4
    * Explanation: The pairs with an absolute difference of 1 are:
    * - [1,2,2,1]
    * - [1,2,2,1]
    * - [1,2,2,1]
    * - [1,2,2,1]
    *
    * Example 2:
    *
    * Input: nums = [1,3], k = 3
    * Output: 0
    * Explanation: There are no pairs with an absolute difference of 3.
    *
    * Example 3:
    *
    * Input: nums = [3,2,1,5,4], k = 2
    * Output: 3
    * Explanation: The pairs with an absolute difference of 2 are:
    * - [3,2,1,5,4]
    * - [3,2,1,5,4]
    * - [3,2,1,5,4]
    *
    * Constraints:
    *
    *    1 <= nums.length <= 200
    *    1 <= nums[i] <= 100
    *    1 <= k <= 99
    */
  def countKDifference(nums: Array[Int], k: Int): Int = {
    (for
      i <- nums.indices
      j <- 0 until i
    yield if math.abs(nums(i) - nums(j)) == k then 1 else 0).sum

  }

  /** 1464. Maximum product of two elements in an array.
    *
    * Given the array of integers nums, you will choose two different indices i and j of that array. Return the maximum value of (nums[i]-1)*(nums[j]-1).
    *
    * Example 1:
    *
    * Input: nums = [3,4,5,2]
    * Output: 12
    * Explanation: If you choose the indices i=1 and j=2 (indexed from 0), you will get the maximum value, that is, (nums[1]-1)*(nums[2]-1) = (4-1)*(5-1) = 3*4 = 12.
    *
    * Example 2:
    *
    * Input: nums = [1,5,4,5]
    * Output: 16
    * Explanation: Choosing the indices i=1 and j=3 (indexed from 0), you will get the maximum value of (5-1)*(5-1) = 16.
    *
    * Example 3:
    *
    * Input: nums = [3,7]
    * Output: 12
    *
    * Constraints:
    *
    *    2 <= nums.length <= 500
    *    1 <= nums[i] <= 10^3
    */
  def maxProduct(nums: Array[Int]): Int = {
    ???
  }

  /** 1. Two sum.
    * Given an array of integers nums and an integer target, return indices of the two numbers such that they add up to target.
    * You may assume that each input would have exactly one solution, and you may not use the same element twice.
    * You can return the answer in any order.
    *
    * Example 1:
    *
    * Input: nums = [2,7,11,15], target = 9
    * Output: [0,1]
    * Explanation: Because nums[0] + nums[1] == 9, we return [0, 1].
    *
    * Example 2:
    *
    * Input: nums = [3,2,4], target = 6
    * Output: [1,2]
    *
    * Example 3:
    *
    * Input: nums = [3,3], target = 6
    * Output: [0,1]
    *
    * Constraints:
    *
    *    - 2 <= nums.length <= 10^4
    *    - -109 <= nums[i] <= 10^9
    *    - -109 <= target <= 10^9
    *    - Only one valid answer exists.
    */

  def twoSum(nums: Array[Int], target: Int): Array[Int] = {

    @scala.annotation.tailrec
    def traverse(i: Int, state: Map[Int, Int]): Array[Int] =
      if state.contains(target - nums(i)) then Array(state(target - nums(i)), i)
      else traverse(i + 1, state + (nums(i) -> i))

    traverse(0, Map.empty[Int, Int])

  }

  /** 15. 3Sum.
    * Given an integer array nums, return all the triplets [nums[i], nums[j], nums[k]] such that i != j, i != k, and j != k, and nums[i] + nums[j] + nums[k] == 0.
    * Notice that the solution set must not contain duplicate triplets.
    *
    * Example 1:
    *
    * Input: nums = [-1,0,1,2,-1,-4]
    * Output: [[-1,-1,2],[-1,0,1]]
    * Explanation:
    * nums[0] + nums[1] + nums[2] = (-1) + 0 + 1 = 0.
    * nums[1] + nums[2] + nums[4] = 0 + 1 + (-1) = 0.
    * nums[0] + nums[3] + nums[4] = (-1) + 2 + (-1) = 0.
    * The distinct triplets are [-1,0,1] and [-1,-1,2].
    * Notice that the order of the output and the order of the triplets does not matter.
    *
    * Example 2:
    *
    * Input: nums = [0,1,1]
    * Output: []
    * Explanation: The only possible triplet does not sum up to 0.
    *
    * Example 3:
    *
    * Input: nums = [0,0,0]
    * Output: [[0,0,0]]
    * Explanation: The only possible triplet sums up to 0.
    *
    * Constraints:
    *
    *    - 3 <= nums.length <= 3000
    *    - -10^5 <= nums[i] <= 10^5
    */

  def threeSum(nums: Array[Int]): List[List[Int]] = {
    val map = nums.zipWithIndex.toMap
    (for
      i <- LazyList.range(0, nums.length - 2)
      j <- LazyList.range(i + 1, nums.length - 1)
      iplusj = nums(i) + nums(j)
      l = List(nums(i), nums(j), -iplusj).sorted
      if map.contains(-iplusj) && map(-iplusj) != i && map(-iplusj) != j
    yield l).distinct.toList

  }

  /** 27. Remove element.
    *
    * Given an integer array nums and an integer val, remove all occurrences of val in nums in-place. The order of the elements may be changed. Then return the number of elements in nums which are not equal to val.
    *
    * Consider the number of elements in nums which are not equal to val be k, to get accepted, you need to do the following things:
    *
    *    Change the array nums such that the first k elements of nums contain the elements which are not equal to val. The remaining elements of nums are not important as well as the size of nums.
    *    Return k.
    *
    * Custom Judge:
    *
    * The judge will test your solution with the following code:
    * {{{
    * int[] nums = [...]; // Input array
    * int val = ...; // Value to remove
    * int[] expectedNums = [...]; // The expected answer with correct length.
    *                            // It is sorted with no values equaling val.
    *
    * int k = removeElement(nums, val); // Calls your implementation
    *
    * assert k == expectedNums.length;
    * sort(nums, 0, k); // Sort the first k elements of nums
    * for (int i = 0; i < actualLength; i++) {
    *    assert nums[i] == expectedNums[i];
    * }
    * }}}
    * If all assertions pass, then your solution will be accepted.
    *
    * Example 1:
    *
    * Input: nums = [3,2,2,3], val = 3
    * Output: 2, nums = [2,2,_,_]
    * Explanation: Your function should return k = 2, with the first two elements of nums being 2.
    * It does not matter what you leave beyond the returned k (hence they are underscores).
    *
    * Example 2:
    *
    * Input: nums = [0,1,2,2,3,0,4,2], val = 2
    * Output: 5, nums = [0,1,4,0,3,_,_,_]
    * Explanation: Your function should return k = 5, with the first five elements of nums containing 0, 0, 1, 3, and 4.
    * Note that the five elements can be returned in any order.
    * It does not matter what you leave beyond the returned k (hence they are underscores).
    *
    * Constraints:
    *
    *    0 <= nums.length <= 100
    *    0 <= nums[i] <= 50
    *    0 <= val <= 100
    */
  def removeElement(nums: Array[Int], `val`: Int): Int = {

    @scala.annotation.tailrec
    def loop(slow: Int, fast: Int): Int =
      if fast == nums.length then slow
      else if nums(slow) != `val` then loop(slow + 1, fast + 1)
      else if nums(fast) == `val` then loop(slow, fast + 1)
      else
        val tmp = nums(fast)
        nums(fast) = nums(slow)
        nums(slow) = tmp
        loop(slow + 1, fast + 1)

    loop(slow = 0, fast = 0)
  }

  /** 26. Remove duplicates from sorted array.
    *
    * Given an integer array nums sorted in non-decreasing order, remove the duplicates in-place such that each unique element appears only once. The relative order of the elements should be kept the same. Then return the number of unique elements in nums.
    *
    * Consider the number of unique elements of nums to be k, to get accepted, you need to do the following things:
    *
    *    Change the array nums such that the first k elements of nums contain the unique elements in the order they were present in nums initially. The remaining elements of nums are not important as well as the size of nums.
    *    Return k.
    *
    * Custom Judge:
    *
    * The judge will test your solution with the following code:
    *
    * int[] nums = [...]; // Input array
    * int[] expectedNums = [...]; // The expected answer with correct length
    *
    * int k = removeDuplicates(nums); // Calls your implementation
    *
    * assert k == expectedNums.length;
    * for (int i = 0; i < k; i++) {
    *    assert nums[i] == expectedNums[i];
    * }
    *
    * If all assertions pass, then your solution will be accepted.
    *
    * Example 1:
    *
    * Input: nums = [1,1,2]
    * Output: 2, nums = [1,2,_]
    * Explanation: Your function should return k = 2, with the first two elements of nums being 1 and 2 respectively.
    * It does not matter what you leave beyond the returned k (hence they are underscores).
    *
    * Example 2:
    *
    * Input: nums = [0,0,1,1,1,2,2,3,3,4]
    * Output: 5, nums = [0,1,2,3,4,_,_,_,_,_]
    * Explanation: Your function should return k = 5, with the first five elements of nums being 0, 1, 2, 3, and 4 respectively.
    * It does not matter what you leave beyond the returned k (hence they are underscores).
    *
    * Constraints:
    *
    *    - 1 <= nums.length <= 3 * 10^4
    *    - -100 <= nums[i] <= 100
    *    - nums is sorted in non-decreasing order.
    */

  def removeDuplicates(nums: Array[Int]): Int = {
    val d = nums.distinct
    d.copyToArray(nums)
    d.size
  }

  /**  136. Single number.
    *
    * Given a non-empty array of integers nums, every element appears twice except for one. Find that single one.
    *
    * You must implement a solution with a linear runtime complexity and use only constant extra space.
    *
    * Example 1:
    *
    * Input: nums = [2,2,1]
    * Output: 1
    *
    * Example 2:
    *
    * Input: nums = [4,1,2,1,2]
    * Output: 4
    *
    * Example 3:
    *
    * Input: nums = [1]
    * Output: 1
    *
    * Constraints:
    *
    *    1 <= nums.length <= 3 * 104
    *    -3 * 104 <= nums[i] <= 3 * 104
    *    Each element in the array appears twice except for one element which appears only once.
    */
  def singleNumber(nums: Array[Int]): Int = {
    nums.reduce(_ ^ _)
  }

  /**  217. Contains duplicate.
    *
    * Given an integer array nums, return true if any value appears at least twice in the array, and return false if every element is distinct.
    *
    * Example 1:
    *
    * Input: nums = [1,2,3,1]
    * Output: true
    *
    * Example 2:
    *
    * Input: nums = [1,2,3,4]
    * Output: false
    *
    * Example 3:
    *
    * Input: nums = [1,1,1,3,3,4,3,2,4,2]
    * Output: true
    *
    * Constraints:
    *
    *    - 1 <= nums.length <= 10^5
    *    -109 <= nums[i] <= 10^9
    */

  def containsDuplicate(nums: Array[Int]): Boolean = {
    nums.size != nums.distinct.size
  }

  /** 409. Longest Palindrome
    *
    * Given a string s which consists of lowercase or uppercase letters, return the length of the longest
    * palindrome
    * that can be built with those letters.
    *
    * Letters are case sensitive, for example, "Aa" is not considered a palindrome.
    *
    * Example 1:
    *
    * Input: s = "abccccdd"
    * Output: 7
    * Explanation: One longest palindrome that can be built is "dccaccd", whose length is 7.
    * Example 2:
    *
    * Input: s = "a"
    * Output: 1
    * Explanation: The longest palindrome that can be built is "a", whose length is 1.
    *
    * Constraints:
    *
    * 1 <= s.length <= 2000
    * s consists of lowercase and/or uppercase English letters only.
    */
  def longestPalindrome(s: String): Int = {
    s.groupBy(identity)
      .view
      .values
      .map(_.size)
      .partition(_ % 2 == 0) match
      case (even, odd) if odd.isEmpty => even.sum
      case (even, odd)                => odd.sum - odd.size + 1 + even.sum
  }
