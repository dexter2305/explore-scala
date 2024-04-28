package leetcode

import scala.compiletime.ops.string

/** Implementations for problems on string.
  */
object StringProblems:
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
    * Input: sentences = ["alice and bob love leetcode", "i think so too", "this is great thanks
    * very much"] Output: 6 Explanation: \- The first sentence, "alice and bob love leetcode", has 5
    * words in total. \- The second sentence, "i think so too", has 4 words in total. \- The third
    * sentence, "this is great thanks very much", has 6 words in total. Thus, the maximum number of
    * words in a single sentence comes from the third sentence, which has 6 words.
    *
    * Example 2:
    *
    * Input: sentences = ["please wait", "continue to fight", "continue to win"] Output: 3
    * Explanation: It is possible that multiple sentences contain the same number of words. In this
    * example, the second and third sentences (underlined) have the same number of words.
    *
    * Constraints:
    *
    * 1 <= sentences.length <= 100 1 <= sentences[i].length <= 100 sentences[i] consists only of
    * lowercase English letters and ' ' only. sentences[i] does not have leading or trailing spaces.
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
    * Input: word = "USA" Output: true
    *
    * Example 2:
    *
    * Input: word = "FlaG" Output: false
    *
    * Constraints:
    *
    * 1 <= word.length <= 100 word consists of lowercase and uppercase English letters.
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
    * Input: jewels = "aA", stones = "aAAbbbb" Output: 3
    *
    * Example 2:
    *
    * Input: jewels = "z", stones = "ZZ" Output: 0
    *
    * Constraints:
    *
    * 1 <= jewels.length, stones.length <= 50 jewels and stones consist of only English letters. All
    * the characters of jewels are unique.
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
    * Input: operations = ["--X","X++","X++"] Output: 1 Explanation: The operations are performed as
    * follows: Initially, X = 0. --X: X is decremented by 1, X = 0 - 1 = -1. X++: X is incremented
    * by 1, X = -1 + 1 = 0. X++: X is incremented by 1, X = 0 + 1 = 1.
    *
    * Example 2:
    *
    * Input: operations = ["++X","++X","X++"] Output: 3 Explanation: The operations are performed as
    * follows: Initially, X = 0.
    * ++X: X is incremented by 1, X = 0 + 1 = 1.
    * ++X: X is incremented by 1, X = 1 + 1 = 2. X++: X is incremented by 1, X = 2 + 1 = 3.
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
    * Input: paths = [["London","New York"],["New York","Lima"],["Lima","Sao Paulo"]] Output: "Sao
    * Paulo" Explanation: Starting at "London" city you will reach "Sao Paulo" city which is the
    * destination city. Your trip consist of: "London" -> "New York" -> "Lima" -> "Sao Paulo".
    *
    * Example 2:
    *
    * Input: paths = [["B","C"],["D","B"],["C","A"]] Output: "A" Explanation: All possible trips
    * are: "D" -> "B" -> "C" -> "A". "B" -> "C" -> "A". "C" -> "A". "A". Clearly the destination
    * city is "A".
    *
    * Example 3:
    *
    * Input: paths = \\[\\["A","Z"\\]\\] Output: "Z"
    *
    * Constraints:
    *
    * 1 <= paths.length <= 100 paths[i].length == 2 1 <= cityAi.length, cityBi.length <= 10 cityAi
    * != cityBi All strings consist of lowercase and uppercase English letters and the space
    * character.
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
    * Input: sentence = "cat and dog" Output: 3 Explanation: The valid words in the sentence are
    * "cat", "and", and "dog".
    *
    * Example 2:
    *
    * Input: sentence = "!this 1-s b8d!" Output: 0 Explanation: There are no valid words in the
    * sentence. "!this" is invalid because it starts with a punctuation mark. "1-s" and "b8d" are
    * invalid because they contain digits.
    *
    * Example 3:
    *
    * Input: sentence = "alice and bob are playing stone-game10" Output: 5 Explanation: The valid
    * words in the sentence are "alice", "and", "bob", "are", and "playing". "stone-game10" is
    * invalid because it contains digits.
    *
    * Constraints:
    *
    * 1 <= sentence.length <= 1000 sentence only contains lowercase English letters, digits, ' ',
    * '-', '!', '.', and ','. There will be at least 1 token.
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
    * The first group consists of the first k characters of the string, the second group consists of
    * the next k characters of the string, and so on. Each character can be a part of exactly one
    * group. For the last group, if the string does not have k characters remaining, a character
    * fill is used to complete the group.
    *
    * Note that the partition is done so that after removing the fill character from the last group
    * (if it exists) and concatenating all the groups in order, the resultant string should be s.
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
    * Given an array of strings words, return the first palindromic string in the array. If there is
    * no such string, return an empty string "".
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
      def helper(head: Int = 0, tail: Int): Boolean = (head, tail) match
        case (h, t) if h <= t => s(h) == s(t) && helper(h + 1, t - 1)
        case _                => true
      helper(tail = s.length - 1)

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
    * 1 <= s.length <= 2 * 105 s consists only of printable ASCII characters.
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
    * Input: word1 = "abc", word2 = "pqr" Output: "apbqcr" Explanation: The merged string will be
    * merged as so: word1: a b c word2: p q r merged: a p b q c r
    *
    * Example 2:
    *
    * Input: word1 = "ab", word2 = "pqrs" Output: "apbqrs" Explanation: Notice that as word2 is
    * longer, "rs" is appended to the end. word1: a b word2: p q r s merged: a p b q r s
    *
    * Example 3:
    *
    * Input: word1 = "abcd", word2 = "pq" Output: "apbqcd" Explanation: Notice that as word1 is
    * longer, "cd" is appended to the end. word1: a b c d word2: p q merged: a p b q c d
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
    */
  def makeGood(s: String): String =
    // using stack is a much optimized and readable solution yet this is done for the heck of it.
    // implemented as I was struggling to implement for 2 hours after solving quickly on paper.
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
    *
    *   - 1 <= haystack.length, needle.length <= 104
    *   - haystack and needle consist of only lowercase english characters.
    *
    * @note
    *   programming-skills
    */
  def strStr(haystack: String, needle: String): Int =
    haystack.indexOf(needle)

  /** 20. Valid parantheses.
    *
    * Given a string s containing just the characters '(', ')', '{', '}', '[' and ']', determine if
    * the input string is valid.
    *
    * An input string is valid if:
    *
    * Open brackets must be closed by the same type of brackets. Open brackets must be closed in the
    * correct order. Every close bracket has a corresponding open bracket of the same type.
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
    */
  def minRemoveToMakeValid(s: String): String =
    // scan left to right and ignore excess right parantheses
    // scan right to left and ignore excess left parantheses
    val (leftFixedReversedList, _) =
      s.foldLeft(List.empty[Char], 0): (t, char) =>
        (t, char) match
          case ((acc, l), '(') => ('(' :: acc, l + 1)
          case ((acc, l), ')') => if l > 0 then (')' :: acc, l - 1) else (acc, l)
          case ((acc, l), c)   => (c :: acc, l)

    val (rFixedList, _) =
      leftFixedReversedList.foldLeft(List.empty[Char], 0): (t, char) =>
        (t, char) match
          case ((acc, r), '(') => if r > 0 then ('(' :: acc, r - 1) else (acc, r)
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
    * For example, if word = "abcdefd" and ch = "d", then you should reverse the segment that starts
    * at 0 and ends at 3 (inclusive). The resulting string will be "dcbaefd".
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
    * different from the last used key. For example, s = "ab" has a change of a key while s = "bBBb"
    * does not have any.
    *
    * Return the number of times the user had to change the key.
    *
    * Note: Modifiers like shift or caps lock won't be counted in changing the key that is if a user
    * typed the letter 'a' and then the letter 'A' then it will not be considered as a changing of
    * key.
    *
    * Example 1:
    *
    * Input: s = "aAbBcC" Output: 2 Explanation: From s[0] = 'a' to s[1] = 'A', there is no change
    * of key as caps lock or shift is not counted. From s[1] = 'A' to s[2] = 'b', there is a change
    * of key. From s[2] = 'b' to s[3] = 'B', there is no change of key as caps lock or shift is not
    * counted. From s[3] = 'B' to s[4] = 'c', there is a change of key. From s[4] = 'c' to s[5] =
    * 'C', there is no change of key as caps lock or shift is not counted.
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
    * Input: word = "abcc" Output: true Explanation: Select index 3 and delete it: word becomes
    * "abc" and each character has a frequency of 1.
    *
    * Example 2:
    *
    * Input: word = "aazz" Output: false Explanation: We must delete a character, so either the
    * frequency of "a" is 1 and the frequency of "z" is 2, or vice versa. It is impossible to make
    * all present letters have equal frequency.
    *
    * Constraints:
    *
    * 2 <= word.length <= 100 word consists of lowercase English letters only.
    */
  def equalFrequency(word: String): Boolean =
    lazy val frequency: String => Map[Char, Int] = word =>
      word
        .groupBy(identity)
        .map: (char, string) =>
          (char, string.size)
    word.toCharArray().toList match
      case Nil => false
      case head :: tail =>
        frequency(tail.mkString).values.toSet.size == 0 || equalFrequency(tail.mkString)

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
    * Approach:
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
    * ===Approach===
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
