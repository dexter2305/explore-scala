package leetcode

import scala.annotation.tailrec
import scala.collection.StringOps
import scala.compiletime.ops.string

object StringProblems:

  def maxNumberOfWords(sentences: Array[String]): Int =
    sentences.map(_.count(_ == ' ') + 1).max

  def defangingIPAddress(address: String): String =
    address.split("\\.").mkString("[.]")

  def toLowerCase(s: String): String =
    s.map: c =>
      if c >= 'A' && c <= 'Z' then (c + 32).toChar else c

  def capitalizeTheTitle(title: String): String =
    title
      .split(' ')
      .map: s =>
        if s.length() <= 2 then s.toLowerCase()
        else s"${s.head.toUpper}${s.tail.toLowerCase()}"
      .mkString(" ")

  def detectCapital(capital: String): Boolean =
    capital.forall(_.isUpper) ||
      capital.forall(_.isLower) ||
      capital.head.isUpper && capital.tail.forall(_.isLower)

  def jewelsAndStones(jewels: String, stones: String): Int =
    val j = jewels.toSet
    stones.count(j.contains)

  def finalValueAfterOps(ops: Array[String]): Int =
    ops.foldLeft(0): (acc, e) =>
      e match
        case "++X" | "X++" => acc + 1
        case "--X" | "X--" => acc - 1

  def findWordsContaining(words: Array[String], x: Char): List[Int] =
    words.zipWithIndex
      .collect:
        case (word, index) if word contains (x) => index
      .toList

  def arrayStringsAreEqual(word1: Array[String], word2: Array[String]): Boolean =
    word1.mkString == word2.mkString

  def truncateSentence(s: String, k: Int): String = s.split(" ").take(k).mkString(" ")

  def countMatches(items: List[List[String]], ruleKey: String, ruleValue: String): Int =
    val index = ruleKey match
      case "type"  => 0
      case "color" => 1
      case "name"  => 2
    items.count: item =>
      item(index).equals(ruleValue)

  def checkIfPangram(sentence: String): Boolean =
    sentence.groupBy(identity).keySet.size == 26

  def reverseWords(s: String): String =
    s.split(" ").map(_.reverse).mkString(" ")

  def sortSentence(s: String): String =
    s.split(" ")
      .map(s => (s.charAt(s.length - 1).asDigit, s.substring(0, s.length - 1)))
      .sortBy((Int, String) => Int)
      .map:
        case (_, s) => s
      .mkString(" ")

  def finalString(s: String): String =
    s.foldLeft(""): (acc, e) =>
      e match
        case 'i' => acc.reverse
        case _   => s"$acc$e"

  def reverseStr(s: String, k: Int): String =
    s.grouped(2 * k)
      .map: string2k =>
        if string2k.length() < k then string2k.reverse
        else
          val (x, y) = string2k.splitAt(k)
          x.reverse.concat(y)
      .mkString("")

  def isAcronym(words: List[String], s: String): Boolean =
    words.map(_.charAt(0)).mkString("") == s

  def countConsistentStrings(allowed: String, words: Array[String]): Int =
    val d = allowed.toSet
    words.count:
      _.forall(d.contains(_))

  def sortPeople(names: Array[String], heights: Array[Int]): Array[String] =
    names
      .zip(heights)
      .sortWith:
        case ((_, h1), (_, h2)) => h1 > h2
      .map(_._1)

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

  def reverseString(s: Array[Char]): Unit =
    @scala.annotation.tailrec
    def aux(chars: Array[Char], l: Int, r: Int): Unit =
      if l < r then
        chars(l) = (chars(l) + chars(r)).toChar
        chars(r) = (chars(l) - chars(r)).toChar
        chars(l) = (chars(l) - chars(r)).toChar
        aux(chars, l + 1, r - 1)
    aux(s, 0, s.length - 1)

  def destCity(paths: List[List[String]]): String =
    @scala.annotation.tailrec
    def traverse(path: List[String]): String =
      val source = path(0)
      val dest = path(1)
      paths.find(_(0) == dest) match
        case None        => dest
        case Some(nodes) => traverse(nodes)
    traverse(paths(0))

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

  def countValidWords(sentence: String): Int =
    val passesNoDigitRule: String => Boolean = s => s.forall(!_.isDigit)
    val passesHypenationRule: String => Boolean = s =>
      s.charAt(0) != '-' &&
        s.indexOf('-') <= (s.length - 1) &&
        s.indexOf('-') == s.lastIndexOf('-')
    val passPunctuationRule: String => Boolean = s =>
      ",.!"
        .toCharArray()
        .map: p =>
          !s.contains(p) || (s.indexOf(p) == s.length - 1)
        .reduce(_ && _)
    sentence
      .split("\\s+")
      .filter(!_.isEmpty())
      .filter(passesNoDigitRule(_))
      .filter(passesHypenationRule(_))
      .filter(passPunctuationRule(_))
      .size

  def divideString(s: String, k: Int, fill: Char): Array[String] =
    val required = if s.length % k != 0 then k - s.length % k else 0
    s.concat(Array.fill(required)(fill).mkString).grouped(k).toArray

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

  def isPalindrome(s: String): Boolean =
    @scala.annotation.tailrec
    def aux(head: Int = 0, tail: Int): Boolean =
      // println(s"$s => ${s(head)} vs ${s(tail)}")
      if head <= tail then
        (s(head), s(tail)) match
          case (x, y) if x.isLetter && y.isLetter =>
            x.toLower == y.toLower && aux(head + 1, tail - 1)
          case (x, y) if x.isLetter && !y.isLetter  => aux(head, tail - 1)
          case (x, y) if !x.isLetter && y.isLetter  => aux(head + 1, tail)
          case (x, y) if !x.isLetter && !y.isLetter => aux(head + 1, tail - 1)
      else true
    s.trim.isEmpty || aux(head = 0, tail = s.length - 1)

  def countSegments(s: String): Int =
    s.split(' ').filter(!_.isEmpty()).size

  def mergeAlternately(word1: String, word2: String): String =
    word1
      .zipAll(word2, "", "")
      .map: (a, b) =>
        s"$a$b"
      .mkString

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

  def removeTrailingZeros(num: String): String =
    // num.dropRight(num.reverse.takeWhile(_ == '0').length)
    num.reverse.dropWhile(_ == '0').reverse

  def countSeniors(details: Array[String]): Int =
    details
      .map(_.substring(11, 13))
      .map(_.toInt)
      .count(_ > 60)

  def makeGood(s: String): String =
    // using stack is a much optimized and readable solution yet this is done for the heck of it.
    // implemented as I was struggling to implement for 2 hours after solving quickly on paper.
    @scala.annotation.tailrec
    def loop(string: String, i: Int): String =
      // println(s"processing $string from index= $i")
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

  def lengthOfLastWord(s: String): Int =
    "\\s*[\\w]*\\s*$".r
      .findFirstIn(s) match
      case None => 0
      case Some(value) =>
        value.trim.length()

  def strStr(haystack: String, needle: String): Int =
    haystack.indexOf(needle)

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

  def checkValidString(s: String): Boolean =
    def validator(string: String, pc: Int, jc: Int): Boolean =
      string.headOption match
        case None =>
          println(s"pc: $pc, joker: $jc")
          jc >= math.abs(pc)
        case Some('(')                       => validator(string.tail, pc + 1, jc)
        case Some('*')                       => validator(string.tail, pc, jc + 1)
        case Some(')') if pc == 0 && jc == 0 => false
        case Some(')')                       => validator(string.tail, pc - 1, jc)
        case _                               => validator(string.tail, pc, jc)
    validator(s, 0, 0)

  def findTheDifference(s: String, t: String): Char =
    (t.sum - s.sum).toChar

  def reversePrefix(word: String, ch: Char): String =
    if word.contains(ch) then
      val index = word.indexOf(ch)
      word.substring(0, index + 1).reverse.concat(word.substring(index + 1))
    else word
