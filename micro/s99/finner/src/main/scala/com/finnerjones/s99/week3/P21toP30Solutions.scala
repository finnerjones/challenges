package com.finnerjones.s99.week3

import com.finnerjones.s99.week2.P11toP20Solutions.removeAt
/*
 * taken from S-99 website
 * http://aperiodic.net/phil/scala/s-99/
 */
object P21toP30Solutions {

  // P21 - my solution
  def insertAt[A](elem:A,n:Int,l:List[A]):List[A] = {    
    val (beforeNew,afterNew) = l splitAt(n)
    beforeNew ::: elem :: afterNew
  }
  
  // P21 - web solution using pattern matching
  def insertAtV2[A](elem:A,n:Int,l:List[A]):List[A] = l splitAt(n) match {    
    case (beforeNew, afterNew) => beforeNew ::: elem :: afterNew
  }

  // not part of S99
  def rightSignificantDigit[A](n:Double, funcToMultiplyBy10:(Double) => Double) = {
    def countPlaces[A](num:Double, pos:Int):Int = {    
        if (num > 0 && num < 1)  countPlaces(funcToMultiplyBy10(num), pos+1)
        else pos
    }
    countPlaces(n,0)
  }
    

  // not part of S99
  def leftSignificantDigit[A](n:Double, funcToDivideBy10:(Double) => Double) = {
    def countPlaces[A](num:Double, pos:Int):Int = {    
        if (num >= 1)  countPlaces(funcToDivideBy10(num), pos+1)
        else pos
    }
    countPlaces(n,0)
  }

  
  // not part of S99
  def significantDigit[A](n:Double) = {
	if (n >= 1) leftSignificantDigit(n, (num:Double) => num / 10.0)
	else rightSignificantDigit(n, (num:Double) => num * 10.0)
  }
  
  // P22 - my solution
  def range(s:Int,f:Int):List[Int] = {
    var elem:Int = s
    var result:List[Int] = List()
    while (elem <= f ) { 
      result = elem :: result
      elem+=1
    }
    result reverse
  }
  
  // P22 - web solution 1 , built in
  def rangeBuiltIn(s:Int, f:Int):List[Int] =
    List.range(s,f + 1)
  

  // P22 - web solution 2 , recursive
  def rangeRecursive(s:Int, f:Int):List[Int] =
    if (f < s) Nil
    else s :: rangeRecursive(s + 1, f)

    
  // P22 - web solution 3 , tail recursive
  def rangeTailRecursive(s:Int, f:Int):List[Int] = {
  	def rangeR(end:Int, result:List[Int]):List[Int] = {
       if (end < s) result
       else rangeR(end - 1, end::result)
  	}
    rangeR(f, Nil)
  }    
    

  
  def unfoldRight[A,B](s:B)(f:B => Option[(A,B)]): List[A] = 
    f(s) match {
      case None 			=> Nil
      case Some((r,n))		=> r::unfoldRight(n)(f)
  }
  
  // P22 - web solution 4 , functional
  def rangeFunctional(s:Int, f:Int):List[Int] = 
    unfoldRight(s) { n =>
    	if (n > f) None
    	else Some((n, n+1))
  }
  
  // P23 - my solution
  // Note: I should have done random on length of list, not the number of items to select
  def randomSelect[A](n:Int,l:List[A]):List[A] = {
    def randomR(t:Int, remainingElems:List[A], result:List[A]):List[A] = {
      if (t == 0) result
      else {
        val (elemsLeft, elemRemoved) = removeAt(scala.util.Random.nextInt(n),remainingElems)
        randomR(t-1,elemsLeft, elemRemoved::result)
      }
      }
    randomR(n,l,Nil)
   }


  // P23 - web solution 1
  def randomSelectV2[A](n:Int,l:List[A]):List[A] = 
    if (n <= 0 ) Nil
    else {
      val (rest, elem) = removeAt((new util.Random).nextInt(l.length), l)
      elem::randomSelectV2(n - 1, rest)
    }

    // P23 - web solution 2, create Random only once because it is expensive
  def randomSelectV3[A](n:Int,l:List[A]):List[A] = {
    def selectR(n:Int, ls:List[A], r:util.Random):List[A] =
      if (n <= 0) Nil
      else {
        val (rest, elem) = removeAt(r.nextInt(ls.length), ls)
        elem :: selectR(n - 1, rest, r)
      }
    selectR(n, l, new util.Random)
  } 
		  
}