package com.finnerjones.s99.week2

import com.finnerjones.s99.ListUtils._

/*
 * taken from S-99 website
 * http://aperiodic.net/phil/scala/s-99/
 */
object S99SolutionsWeek2 {

  // P11 - my own solution, using pack(l) !!!
  def packModified(l:List[Any]):List[Any] = 
    pack(l) map { e =>
	  if (e.length > 1) (e.length,e.head)
	  else e.head
  	}
  
  
  // P11 - my own solution, this time using encode(l)
  def encodeModified(l:List[Any]):List[Any] =
    encode(l) map { e => if (e._1 > 1) e else e._2}
  
  
  // P11 - web solution 2, more typesafe
  def encodeModified2[A](l:List[A]):List[Either[A, (Int,A)]] =
    encode2(l) map { tuple => if (tuple._1 == 1) Left(tuple._2) else Right(tuple)}
  
  
  // P12 - my own solution !!!
  def decode[A](l:List[(Int,A)]):List[A] = 
    l flatMap { t => List.concat(List.fill(t._1)(t._2)) }
  
  
  // P12 - web solution
  def decode1[A](l:List[(Int,A)]):List[A] =
    l flatMap { e => List.make(e._1, e._2)}
  
  
  // P13 - my first draft solution, implementing pack and encode in one def 
  def encodeDirect[A](l:List[A]):List[(Int,A)] = {
	
    def packD[A](ls:List[A]):List[List[A]] = ls match {
      case Nil => Nil
      case h::tail => ls.takeWhile(_ == ls.head) :: packD(ls dropWhile(_ == ls.head))
    }
    
	val psD = packD(l)
	
	def encodeD[A](ps:List[List[A]]):List[(Int,A)] = ps match {
	  case Nil => Nil
	  case h::tail => (h.length,h.head) :: encodeD(tail)
	}
	  
	encodeD(psD)
  }

  // P13 - second draft solution based on the web pack() P09
  def encodeD2[A](l:List[A]):List[(Int,A)] = {
    
    val (packed, next) = l span { _ == l.head }
    if (next == Nil) List((packed.length,packed.head))
    else List((packed.length,packed.head)):::encodeD2(next)
  }
  
  // P13 - third draft solution base on my pack() solution P09
  def encodeD3[A](l:List[A]):List[(Int,A)] = l match {
    case Nil => Nil
    case h::tail => {
      val ls = (l.takeWhile(_ == l.head))
      List( (ls.length, ls.head)) ::: encodeD3(l dropWhile(_ == l.head))
    }
  }
  
  //P14  - my solution 
  def duplicate[A](l:List[A]):List[A] =
    l flatMap (elem => List(elem,elem))
  
  
  // P15 - my solution
  def duplicateN[A](n:Int, l:List[A]): List[A] =
    l flatMap (elem => List.fill(n)(elem))
  
  // P15 from website
  def duplicate2N[A](n:Int, l:List[A]):List[A] =
    l flatMap { List.make(n, _) }
  
  
  // P16 - my solution
  def drop[A](n:Int,l:List[A]):List[A] = 
    l filter (e => (l.indexOf(e)+1) % 3 != 0)


  // P16 - web solution 1
  def dropRecursive[A](n:Int, l:List[A]):List[A] = {
    def dropR(c:Int, curList:List[A]) : List[A] = (c,curList) match {
      case (_, Nil) => Nil
      case (1, _::tail) => dropR(n,tail)
      case (_, h::tail) => h :: dropR(c- 1, tail)
    }
    dropR(n,l)
    
  }

  // P16 - web solution 2
  def dropTailRecursive[A](n:Int, l:List[A]):List[A] = {
    def dropR(c:Int, curList:List[A], result:List[A]):List[A] = (c,curList) match {
      case (_, Nil) => result.reverse
      case (1, _::tail) => dropR(n,tail,result)
      case (_, h::tail) => dropR(c - 1, tail, h::result)
    }
    dropR(n,l,Nil)
  }

  // P16 - web solution 3 (similar to mine)
  def dropFunctional[A](n:Int, l:List[A]): List[A] =
    l.zipWithIndex filter { v => (v._2 +1) % n != 0} map { _._1 }

  // P17 - my solution
  def split(n:Int, l:List[Any]):(List[Any],List[Any]) =
    ((l slice (0,n),l slice (n,l.length)))

  // P17 - built in first web solution
  def splitBuiltIn[A](n:Int, l:List[A]):(List[A],List[A]) =
    l splitAt (n)

  // P17 - recursive second web solution
  def splitRecursive[A](n:Int, l:List[A]):(List[A],List[A]) = (n,l) match {
    case (_, Nil) => (Nil, Nil)
    case (0, list) => (Nil, list)
    case (n, h :: tail) => {
      val (pre, post) = splitRecursive(n - 1, tail)
      (h :: pre, post)
    }
  }
    
  // P17 - tail recursive second web solution
  def splitTailRecursive[A](n:Int, l:List[A]):(List[A],List[A]) = {
	def splitTailR(curN: Int, curL:List[A], pre:List[A]): (List[A],List[A]) =  (curN, curL) match {
	  case (_, Nil) => (Nil,Nil)
	  case (0, list) => (pre.reverse, list)
	  case (n, h :: tail) => splitTailR(n - 1, tail, h::pre)
	}
	splitTailR(n,l,Nil)
  }

  // P17 - functional third web solution
  def splitFunctional[A](n:Int, l:List[A]):(List[A],List[A]) = {
    (l take (n), l drop (n))
  }

  
  // P18 - already built in
  def slice[A](start:Int, end:Int, l:List[A]): List[A] = 
    l slice (start,end)

  // P18 - my built in functional attempt
  def slice2[A](start:Int, end:Int, l:List[A]): List[A] = 
    (l drop (start)) take (end - start)

  // P18 - my built in functional attempt
  def slice3[A](start:Int, end:Int, l:List[A]): List[A] = 
    l filter (e => (((l indexOf(e)) >= start)  && ((l indexOf(e)) < end)   ))

  // P18 - recursive, second web solution
  def sliceRecursive[A](start:Int, end:Int, l:List[A]): List[A] = (start,end,l) match {
    case (_,_,Nil)                  => Nil
    case (_,e,_) if e <= 0          => Nil
    case (s,e,h::tail) if s <= 0    => h :: sliceRecursive(0, e - 1, tail)
    case (s,e,h::tail)              => sliceRecursive(s -1, e -1, tail)
  } 
    
  // P19 -  my solution
  def rotate[A](pos:Int, l:List[A]):List[A] = (pos,l) match {
    case (_,Nil) => Nil
    case (0,_) => l
    case (p, h::tail) => if (p > 0 ) rotate(p-1,tail:::List(h)) else rotate(p+1, List(tail last):::l init) 
  }
    
  // P19 - the web page solution
  def rotateAlt[A](n:Int, ls:List[A]): List[A] = {
    val nBounded = if (ls.isEmpty) 0 else n % ls.length
    if (nBounded < 0) rotate(nBounded + ls.length,ls)
    else (ls drop nBounded):::(ls take nBounded)
  }
  
  
  // P20
  def removeAt[A](n:Int, l:List[A]):(List[A], A) =
    (l filter (_ != l(n)), l(n))
  
}