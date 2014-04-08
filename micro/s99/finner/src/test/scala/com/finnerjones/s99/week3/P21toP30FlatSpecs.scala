package com.finnerjones.s99.week3

import org.scalatest.FlatSpec
import org.scalatest.Matchers

import com.finnerjones.s99.week3.P21toP30Solutions._
class P21toP30FlatSpecs extends FlatSpec with Matchers {
  
  // P21 - my solution
  "insertAt(\"new\", 1, List('a', 'b', 'c', 'd'))" should
  "return List('a, \"new\", 'b', 'c', 'd')" in {
    val l = List('a', 'b', 'c', 'd')
    insertAt("new", 1, l) should be (List('a',"new",'b', 'c', 'd'))
  }

  // P21 - web solution using pattern matching
  "insertAtV2(\"new\", 2, List('a', 'b', 'c', 'd'))" should
  "return List('a, 'b', \"new\", 'c', 'd')" in {
    val l = List('a', 'b', 'c', 'd')
    insertAt("new", 2, l) should be (List('a','b',"new",'c','d'))
  }

  // Not part of S99
  "rightSignificantDigitLessThanOne(0.0000000000043)" should
  "return (12)" in {
    rightSignificantDigit(0.0000000000043, (num:Double) => num * 10.0) should be (12)
  }
  
  // Not part of S99
  "rightSignificantDigitLessThanOne(0.001)" should
  "return (3)" in {
    rightSignificantDigit(0.001, (num:Double) => num * 10.0) should be (3)
  }

  
  // Not part of S99
  "leftSignificantDigitLessThanOne(1000.10)" should
  "return (4)" in {
    leftSignificantDigit(1000.10, (num:Double) => num / 10.0) should be (4)
  }
  
  // Not part of S99
  "leftSignificantDigitLessThanOne(1.0)" should
  "return (1)" in {
    leftSignificantDigit(1.0, (num:Double) => num / 10.0) should be (1)
  }
  
  // Not part of S99
  "significantDigit(1.0)" should
  "return (1)" in {
    significantDigit(1.0) should be (1)
  }
  
  // Not part of S99
  "significantDigit(0.0009)" should
  "return (4)" in {
    significantDigit(0.0009) should be (4)
  }
  
  
  // P22 - my solution
  "range(4, 9)" should
  "return List(4, 5, 6, 7, 8, 9)" in {
    range(4,9) should be (List(4, 5, 6, 7, 8, 9))
  }


  // P22 - web solution 1 , built in
  "rangeBuiltIn(4, 9)" should
  "return List(4, 5, 6, 7, 8, 9)" in {
    rangeBuiltIn(4,9) should be (List(4, 5, 6, 7, 8, 9))
  }


  // P22 - web solution 2 , recursive
  "rangeRecursive(4, 9)" should
  "return List(4, 5, 6, 7, 8, 9)" in {
    rangeRecursive(4,9) should be (List(4, 5, 6, 7, 8, 9))
  }

  // P22 - web solution 3 , tail recursive
  "rangeTailRecursive(4, 9)" should
  "return List(4, 5, 6, 7, 8, 9)" in {
    rangeTailRecursive(4,9) should be (List(4, 5, 6, 7, 8, 9))
  }


  // P22 - web solution 4 , functional
  "rangeFunctional(4, 9)" should
  "return List(4, 5, 6, 7, 8, 9)" in {
    rangeFunctional(4,9) should be (List(4, 5, 6, 7, 8, 9))
  }
  
  // P23 - my solution
  "randomSelect(3, List('a', 'b', 'c', 'd', 'f', 'g', 'h'))" should
  "return 3 randomly selected elements from List('a', 'b', 'c', 'd', 'f', 'g', 'h')" in {
    val originalList = List('a', 'b', 'c', 'd', 'f', 'g', 'h')
    val randomList = randomSelect(3, originalList) 
    val unselectedList = originalList filterNot randomList.contains
    randomList should have length 3
    //  ------------------------------------------->    TODO: I want to use a List val  
    randomList ::: unselectedList should contain allOf ('a', 'b', 'c', 'd', 'f', 'g', 'h')
  }
  
  // P23 - web solution 1
  "randomSelectV2(4, List('a', 'b', 'c', 'd', 'f', 'g', 'h'))" should
  "return 4 randomly selected elements from List('a', 'b', 'c', 'd', 'f', 'g', 'h')" in {
    val originalList = List('a', 'b', 'c', 'd', 'f', 'g', 'h')
    val randomList = randomSelectV2(4, originalList) 
    val unselectedList = originalList filterNot randomList.contains
    randomList should have length 4
    //  ------------------------------------------->    TODO: I want to use a List val  
    randomList ::: unselectedList should contain allOf ('a', 'b', 'c', 'd', 'f', 'g', 'h')
  }
  
  // P23 - web solution 2
  "randomSelectV3(5, List('a', 'b', 'c', 'd', 'f', 'g', 'h'))" should
  "return 5 randomly selected elements from List('a', 'b', 'c', 'd', 'f', 'g', 'h')" in {
    val originalList = List('a', 'b', 'c', 'd', 'f', 'g', 'h')
    val randomList = randomSelectV3(5, originalList) 
    val unselectedList = originalList filterNot randomList.contains
    randomList should have length 5
    //  ------------------------------------------->    TODO: I want to use a List val  
    randomList ::: unselectedList should contain allOf ('a', 'b', 'c', 'd', 'f', 'g', 'h')
  }
  
  
}