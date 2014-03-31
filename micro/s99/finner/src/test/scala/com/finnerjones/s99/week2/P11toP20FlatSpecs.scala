package com.finnerjones.s99.week2

import org.scalatest.FlatSpec
import org.scalatest.Matchers

import com.finnerjones.s99.week2.P11toP20Solutions._

/*
 * Taken from S-99 problems
 * http://aperiodic.net/phil/scala/s-99/
 */
class FlatSpecsWeek2 extends FlatSpec with Matchers {

  // P11 - using pack(l) from P10
  "packModified(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'))" should
    "return List((4,'a'), 'b', (2,'c'), (2,'a'), 'd', (4,'e'))" in {
      val l = List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')
      packModified(l) should be(List((4, 'a'), 'b', (2, 'c'), (2, 'a'), 'd', (4, 'e')))
    }

  // P11 - using encode(l) from P10
  "encodeModified(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'))" should
    "return List((4,'a'), 'b', (2,'c'), (2,'a'), 'd', (4,'e'))" in {
      val l = List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')
      encodeModified(l) should be(List((4, 'a'), 'b', (2, 'c'), (2, 'a'), 'd', (4, 'e')))
    }

  // P11 - using encode(l) from P10
  "encodeModified2(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'))" should
    "return List((4,'a'), 'b', (2,'c'), (2,'a'), 'd', (4,'e'))" in {
      val l = List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')
      encodeModified2(l) should be(List(Right(4, 'a'), Left('b'), Right(2, 'c'), Right(2, 'a'), Left('d'), Right(4, 'e')))
    }

  // P12 
  "decode(List((4, 'a'), (1, 'b'), (2, 'c'), (2, 'a'), (1, 'd'), (4, 'e')))" should
    "List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')" in {
      val l = List((4, 'a'), (1, 'b'), (2, 'c'), (2, 'a'), (1, 'd'), (4, 'e'))
      decode(l) should be(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'))
    }

  // P12 
  "decode1(List((4, 'a'), (1, 'b'), (2, 'c'), (2, 'a'), (1, 'd'), (4, 'e')))" should
    "List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')" in {
      val l = List((4, 'a'), (1, 'b'), (2, 'c'), (2, 'a'), (1, 'd'), (4, 'e'))
      decode1(l) should be(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'))
    }

  
  // P13 first draft implementing both pack and encode in one def
  "encodeDirect(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'))" should
  "return List((4,'a'), (1,'b'), (2,'c'), (2,'a'), (1,'d'), (4,'e'))" in {
    val l = List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')
    encodeDirect(l) should be (List((4,'a'), (1,'b'), (2,'c'), (2,'a'), (1,'d'), (4,'e')))
  }

  // P13 second draft solution based on the web pack() P09
  "encodeD2(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'))" should
  "return List((4,'a'), (1,'b'), (2,'c'), (2,'a'), (1,'d'), (4,'e'))" in {
    val l = List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')
    encodeD2(l) should be (List((4,'a'), (1,'b'), (2,'c'), (2,'a'), (1,'d'), (4,'e')))
  }

  
  // P13 third draft solution base on my pack() solution P09
  "encodeD3(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'))" should
  "return List((4,'a'), (1,'b'), (2,'c'), (2,'a'), (1,'d'), (4,'e'))" in {
    val l = List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')
    encodeD3(l) should be (List((4,'a'), (1,'b'), (2,'c'), (2,'a'), (1,'d'), (4,'e')))
  }
  
  // P14 - my solution 
  "duplicate(List('a', 'b', 'c', 'c', 'd'))" should 
  "return List('a', 'a', 'b', 'b', 'c', 'c', 'c', 'c', 'd', 'd')" in {
    val l = List('a', 'b', 'c', 'c', 'd')
    duplicate(l) should be (List('a', 'a', 'b', 'b', 'c', 'c', 'c', 'c', 'd', 'd'))
  }
  
  // P15 
  "duplicateN(3, List('a', 'b', 'c', 'c', 'd'))" should
  "return List('a', 'a', 'a', 'b', 'b', 'b', 'c', 'c', 'c', 'c', 'c', 'c', 'd', 'd', 'd')" in {
    val l = List('a', 'b', 'c', 'c', 'd')
    duplicateN(3,l) should be (List('a', 'a', 'a', 'b', 'b', 'b', 'c', 'c', 'c', 'c', 'c', 'c', 'd', 'd', 'd'))
  }
  
  // P15 from website
  "duplicate2N(3, List('a', 'b', 'c', 'c', 'd'))" should
  "return List('a', 'a', 'a', 'b', 'b', 'b', 'c', 'c', 'c', 'c', 'c', 'c', 'd', 'd', 'd')" in {
    val l = List('a', 'b', 'c', 'c', 'd')
    duplicate2N(3,l) should be (List('a', 'a', 'a', 'b', 'b', 'b', 'c', 'c', 'c', 'c', 'c', 'c', 'd', 'd', 'd'))
  }
 
  
  // P16 - my solution
  "drop(3, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k'))" should
  "return List('a', 'b', 'd', 'e', 'g', 'h', 'j', 'k')" in {
    val l = List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')
    drop(3,l) should be (List('a', 'b', 'd', 'e', 'g', 'h', 'j', 'k'))
    
  }
  
  // P16 - web solution recursive
  "dropRecursive(3, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k'))" should
  "return List('a', 'b', 'd', 'e', 'g', 'h', 'j', 'k')" in {
    val l = List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')
    dropRecursive(3,l) should be (List('a', 'b', 'd', 'e', 'g', 'h', 'j', 'k'))
    
  }

  
   // P16 - web solution tail recursive
  "dropTailRecursive(3, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k'))" should
  "return List('a', 'b', 'd', 'e', 'g', 'h', 'j', 'k')" in {
    val l = List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')
    dropTailRecursive(3,l) should be (List('a', 'b', 'd', 'e', 'g', 'h', 'j', 'k'))
  }
 
   // P16 - web solution functional
  "dropFunctional(3, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k'))" should
  "return List('a', 'b', 'd', 'e', 'g', 'h', 'j', 'k')" in {
    val l = List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')
    dropFunctional(3,l) should be (List('a', 'b', 'd', 'e', 'g', 'h', 'j', 'k'))
  }

  
  // P17 - my solution
  "split(3, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k'))" should 
  "return (List('a', 'b', 'c'),List('d', 'e', 'f', 'g', 'h', 'i', 'j', 'k'))" in {
    val l = List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')
    split(3,l) should be ((List('a', 'b', 'c'),List('d', 'e', 'f', 'g', 'h', 'i', 'j', 'k')))
  }
  
  // P17 - built in
  "splitBuiltIn(3, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k'))" should 
  "return (List('a', 'b', 'c'),List('d', 'e', 'f', 'g', 'h', 'i', 'j', 'k'))" in {
    val l = List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')
    splitBuiltIn(3,l) should be ((List('a', 'b', 'c'),List('d', 'e', 'f', 'g', 'h', 'i', 'j', 'k')))
  }
  
  // P17 - recursive
  "splitRecursive(3, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k'))" should 
  "return (List('a', 'b', 'c'),List('d', 'e', 'f', 'g', 'h', 'i', 'j', 'k'))" in {
    val l = List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')
    splitRecursive(3,l) should be ((List('a', 'b', 'c'),List('d', 'e', 'f', 'g', 'h', 'i', 'j', 'k')))
  }
  
  // P17 - tail recursive
  "splitTailRecursive(3, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k'))" should 
  "return (List('a', 'b', 'c'),List('d', 'e', 'f', 'g', 'h', 'i', 'j', 'k'))" in {
    val l = List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')
    splitTailRecursive(3,l) should be ((List('a', 'b', 'c'),List('d', 'e', 'f', 'g', 'h', 'i', 'j', 'k')))
  }


  // P17 - functional
  "splitFunctional(3, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k'))" should 
  "return (List('a', 'b', 'c'),List('d', 'e', 'f', 'g', 'h', 'i', 'j', 'k'))" in {
    val l = List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')
    splitFunctional(3,l) should be ((List('a', 'b', 'c'),List('d', 'e', 'f', 'g', 'h', 'i', 'j', 'k')))
  }
  
  
  // P18 - the built in function
  "slice(3, 7, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k'))" should
  "return (List('d', 'e', 'f', 'g'))" in {
    val l = List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')
    slice(3,7,l) should be (List('d', 'e', 'f', 'g'))
  }
  

  // P18 - functional built in solution
  "slice2(3, 7, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k'))" should
  "return (List('d', 'e', 'f', 'g'))" in {
    val l = List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')
    slice2(3,7,l) should be (List('d', 'e', 'f', 'g'))
  }

  // P18 - the built in function
  "slice3(3, 7, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k'))" should
  "return (List('d', 'e', 'f', 'g'))" in {
    val l = List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')
    slice3(3,7,l) should be (List('d', 'e', 'f', 'g'))
  }
  
  // P18 - recursive solution from web
  "sliceFunc(3, 7, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k'))" should
  "return (List('d', 'e', 'f', 'g'))" in {
    val l = List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')
    sliceRecursive(3,7,l) should be (List('d', 'e', 'f', 'g'))
  }
  
  // P19 - my solution 
  "rotate(3, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k'))" should
  "return (List('d', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'a', 'b', 'c'))" in {
    val l = List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')
    rotate(3,l) should be (List('d', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'a', 'b', 'c'))
  }

  // P19 - my solution
  "rotate(5, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k'))" should
  "return (List('f', 'g', 'h', 'i', 'j', 'k', 'a', 'b', 'c','d', 'e'))" in {
    val l = List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')
    rotate(5,l) should be (List('f', 'g', 'h', 'i', 'j', 'k', 'a', 'b', 'c','d', 'e'))
  }

  // P19 - my solution
  "rotate(-2, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k'))" should
  "return (List('j', 'k', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i'))" in {
    val l = List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')
    rotate(-2,l) should be (List('j', 'k', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i'))
  }
  

  // P19 - my solution
  "rotate(-4, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k'))" should
  "return (List('h', 'i','j', 'k', 'a', 'b', 'c', 'd', 'e', 'f', 'g'))" in {
    val l = List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')
    rotate(-4,l) should be (List('h', 'i','j', 'k', 'a', 'b', 'c', 'd', 'e', 'f', 'g'))
  }

  // P19 - web solution 
  "rotateAlt(3, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k'))" should
  "return (List('d', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'a', 'b', 'c'))" in {
    val l = List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')
    rotateAlt(3,l) should be (List('d', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'a', 'b', 'c'))
  }

  // P19 - web solution
  "rotateAlt(-2, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k'))" should
  "return (List('j', 'k', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i'))" in {
    val l = List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')
    rotateAlt(-2,l) should be (List('j', 'k', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i'))
  }
  
  
  // P20 - my solution
  "removeAt(1, List('a', 'b', 'c', 'd'))" should
  "return (List('a', 'c', 'd'),'b')" in {
    val l = List('a', 'b', 'c', 'd')
    removeAt(1,l) should be (List('a', 'c', 'd'),'b')
  }
  
  // P20 - web solution1
  "removeAtV2(1, List('a', 'b', 'c', 'd'))" should
  "return (List('a', 'c', 'd'),'b')" in {
    val l = List('a', 'b', 'c', 'd')
    removeAtV2(1,l) should be (List('a', 'c', 'd'),'b')
  }
  
  // P20 - web solution1
  "removeAtV3(1, List('a', 'b', 'c', 'd'))" should
  "return (List('a', 'c', 'd'),'b')" in {
    val l = List('a', 'b', 'c', 'd')
    removeAtV3(1,l) should be (List('a', 'c', 'd'),'b')
  }
  
  
}