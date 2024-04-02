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

end StringProblems
