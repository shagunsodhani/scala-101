package Intset_class

object intset{
	val t1 = new NonEmpty(3, new Empty, new Empty)
                                                  //> t1  : Intset_class.NonEmpty =  . { . 3 . } . 
	t1 contains 2                             //> res0: Boolean = false
  val t2 = 	t1 incl 2                         //> t2  : Intset_class.Intset =  . { . { . 2 . } . 3 . } . 
	t2 contains 2                             //> res1: Boolean = true
  val t3 = t1 union t2                            //> t3  : Intset_class.Intset =  . { . { . 2 . } . 3 . } . 
  
}


abstract class Intset {
	def incl (x:Int) : Intset
	def contains (x:Int) : Boolean
	def union (other:Intset) : Intset
	
}


class Empty extends Intset {
	def contains (x:Int) : Boolean = false
	def incl (x:Int) : Intset = new NonEmpty(x, new Empty, new Empty)
	override def toString = " . "
	def union (other:Intset) : Intset = other
	
}


class NonEmpty (elem: Int, left: Intset, right: Intset) extends Intset {
	def contains (x:Int) : Boolean =
		if (x < elem) left contains x
		else if (x > elem) right contains x
		else true
		
	def incl (x: Int) : Intset =
		if (x < elem) new NonEmpty(elem, left incl x, right)
		else if (x > elem) new NonEmpty(elem, left, right incl x)
		else this
		
	override def toString = " . {" + left + elem + right + "} . "

	def union (other:Intset) : Intset = ( ( left union right ) union other ) incl elem
	
	
	}