/** A list (singly-linked, immutable).
*  Similar to the built-in lists, but spelled out for pedagogic reasons.
*  (Don't use this class for anything real; use Scala’s built-in lists!)
*
* @author Stacy Austin
* @version 2018-Feb-12
* @license: CC-BY 4.0 -- you are free to share and adapt this material
* for any purpose, provided you include appropriate attribution.
*   https://creativecommons.org/licenses/by/4.0/
 *   https://creativecommons.org/licenses/by/4.0/legalcode
 * Including the source material's URL satisfies "appropriate attribution".
*
*
* An SList[T] is either:
*    - Empty, or
*    - SList( datum:T, rest:SList[T] )
*
* Immutable, using Scala’s "case classes".
* We'll be non-O.O., and have 'static' methods `size`, `contains`, etc.
*/
 
abstract class SList[T]
                                                          
case class Empty[T]() extends SList[T]
 
case class Cons[T]( datum:T, rest:SList[T] ) extends SList[T]
 
 
 
 
 
/** @return the number of elements in `lst` */
def size[T]( lst : SList[T] ) : Int = lst match {
    case Empty() => 0
    case Cons(d,r) => 1+size(r)
    }
 
/** @return whether `target` occurs in `lst` */
def contains[T]( lst : SList[T], target : T ) : Boolean = lst match {
    case Empty() => false
    case Cons(d,r) =>  target.equals(d) || contains(r, target)
    }
 
/** @return the total length of all strings in "lst".
  */
 
def sumStringLengths[T]( lst : SList[T]) : Int = lst match {
    case Empty() => 0
    case Cons(d,r) =>  d.toString.length() + sumStringLengths(r)
    }
 
//************************ a lowbrow testing framework ************************/
 
private var testNum  : Int = 0
private var spaceNum : Int = 0
private val groupSize = 5
private val lineSize  = 10*groupSize
 
def test( actual:Any, expected:Any ) : Unit = {
    testNum += 1
    spaceNum += 1
    if (actual.equals(expected))
        System.out.printf(".")
    else {
        System.out.printf("!")
        System.err.printf("TEST #%d FAILED:\n%s\nnot equal to expected\n%s\n",
                          testNum : Integer,
                          actual.toString(),
                          expected.toString())
        }
    if (spaceNum%groupSize==0) { System.out.printf(" ") }
    if (spaceNum%lineSize==0 ) { System.out.printf("\n"); spaceNum=0 }
    }
 
def printTestMsg( msg:String ) : Unit = {
    System.out.printf( "%s%s\n", if (spaceNum!=0) "\n" else "", msg )
    spaceNum = 0
    }
 
//************************ end lowbrow testing framework ************************/
 
 
 
 
//************************ run tests ************************/
 
printTestMsg("Starting tests")
 
val l0  = Empty() : SList[String];
val l1  = Cons( "hi", l0 );
val l2  = Cons( "bonjour", l1 );
val l3  = Cons( "bueno", l2 );
val l3b = Cons( "buenos dias", l2 );
val l4  = Cons( "aloha", l3);

 
test(sumStringLengths(l0), 0);                       
test(sumStringLengths(l1), 2);                       
test(sumStringLengths(l2), 9);                       
                        
 
 
 
test( size(Empty()), 0 )
test( size(l1), 1 )
test( size(l2), 2 )
test( size(l4), 4 )
 
test( contains( l0, "hi" ),      false )
 
test( contains( l1, "hi" ),      true  )
test( contains( l1, "bonjour" ), false )
 
test( contains( l2, "hi" ),      true  )
test( contains( l2, "bonjour" ), true  )
test( contains( l2, "aloha" ),   false )
 
test( contains( l4, "bonjour" ), true  )
test( contains( l4, "aloha" ),   true  )
test( contains( l4, "howdy" ),   false )
 
printTestMsg("Finished tests")