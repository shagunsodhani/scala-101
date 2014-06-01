object list_itself {

val fruit: List[String] = List("apples", "oranges", "pears")
                                                  //> fruit  : List[String] = List(apples, oranges, pears)
                
fruit.head                                        //> res0: String = apples

fruit.tail.head                                   //> res1: String = oranges

// List of Integers
val nums: List[Int] = List(5, 4, 3, 2, 1)         //> nums  : List[Int] = List(5, 4, 3, 2, 1)

def isort(xs: List[Int]) : List[Int] = xs match {
	case List() => List()
	case y :: ys => insert(y, isort(ys))
}                                                 //> isort: (xs: List[Int])List[Int]

def insert(x : Int, xs: List[Int] ) : List[Int] = xs match {
	case List() => List(x)
	case y :: ys => if (x <= y) x::y::ys else y :: insert(x, isort(ys))
	
}                                                 //> insert: (x: Int, xs: List[Int])List[Int]

def sortnum : List[Int] = isort(nums)             //> sortnum: => List[Int]

def last[T](xs: List[T]): T = xs match {
	case List() => throw new Error("Last of empty list")
	case List(x) => x
	case y :: ys => last(ys)
}                                                 //> last: [T](xs: List[T])T

def init[T](xs : List[T]) : List[T] = xs match {
	case List() => throw new Error ("Init of empty list")
	case List(x) => Nil
	case y :: ys => y :: init(ys)
}                                                 //> init: [T](xs: List[T])List[T]

def concat[T](xs: List[T], ys: List[T]) : List[T] = xs match {
 case List() => ys
 case z  :: zs => z :: concat(zs, ys)
}                                                 //> concat: [T](xs: List[T], ys: List[T])List[T]

def reverse[T](xs : List[T]) : List[T] = xs match {
	case List() => List()
	case y :: ys => reverse(ys) ++ List(y)
}                                                 //> reverse: [T](xs: List[T])List[T]

def msort (xs : List[Int] ): List[Int] = {
	val n = xs.length/2
	if (n==0) xs
	else {
		def merge(xs : List[Int], ys : List[Int] ): List[Int] = xs match {
			case List() => ys
			case x :: xs1 => ys match {
				case List() => xs
				case y :: ys1 =>
					if (x < y) x :: merge(xs1, ys)
					else y :: merge (xs, ys1)
			}
		}
		val (fst, snd) = xs splitAt n
		merge(msort(fst), msort(snd))
	}
}                                                 //> msort: (xs: List[Int])List[Int]

msort(nums)                                       //> res2: List[Int] = List(1, 2, 3, 4, 5)

def removeAt[T](xs: List[T], n: Int) : List[T] = (xs take n) ::: (xs drop n+1)
                                                  //> removeAt: [T](xs: List[T], n: Int)List[T]
removeAt(sortnum, 1)                              //> res3: List[Int] = List(1, 3, 4, 5)
sortnum                                           //> res4: List[Int] = List(1, 2, 3, 4, 5)
reverse (sortnum)                                 //> res5: List[Int] = List(5, 4, 3, 2, 1)

concat(sortnum, nums)                             //> res6: List[Int] = List(1, 2, 3, 4, 5, 5, 4, 3, 2, 1)
init(sortnum)                                     //> res7: List[Int] = List(1, 2, 3, 4)

last(sortnum)                                     //> res8: Int = 5
sortnum                                           //> res9: List[Int] = List(1, 2, 3, 4, 5)
sortnum updated (2,2)                             //> res10: List[Int] = List(1, 2, 2, 4, 5)

sortnum.contains(5)                               //> res11: Boolean = true
sortnum contains 25                               //> res12: Boolean = false
sortnum indexOf 25                                //> res13: Int = -1

isort(sortnum++nums.reverse)                      //> res14: List[Int] = List(1, 1, 2, 2, 3, 3, 4, 4, 5, 5)
nums.length                                       //> res15: Int = 5
nums.last                                         //> res16: Int = 1
nums.init                                         //> res17: List[Int] = List(5, 4, 3, 2)
nums(2)                                           //> res18: Int = 3
nums apply 2                                      //> res19: Int = 3

nums take 2                                       //> res20: List[Int] = List(5, 4)
nums.init drop 2                                  //> res21: List[Int] = List(3, 2)

val nums1 = Nil.::(4).::(3).::(2).::(1)           //> nums1  : List[Int] = List(1, 2, 3, 4)

val nums2 = 1 :: 2 :: 3 :: 4 :: Nil               //> nums2  : List[Int] = List(1, 2, 3, 4)
// Empty List.
val empty: List[Nothing] = List()                 //> empty  : List[Nothing] = List()

// Two dimensional list
val dim: List[List[Int]] =
   List(
      List(1, 0, 0),
      List(0, 1, 0),
      List(0, 0, 1)
   )                                              //> dim  : List[List[Int]] = List(List(1, 0, 0), List(0, 1, 0), List(0, 0, 1))
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
}