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

init(sortnum)                                     //> res2: List[Int] = List(1, 2, 3, 4)

last(sortnum)                                     //> res3: Int = 5
sortnum                                           //> res4: List[Int] = List(1, 2, 3, 4, 5)
sortnum updated (2,2)                             //> res5: List[Int] = List(1, 2, 2, 4, 5)

sortnum.contains(5)                               //> res6: Boolean = true
sortnum contains 25                               //> res7: Boolean = false
sortnum indexOf 25                                //> res8: Int = -1

isort(sortnum++nums.reverse)                      //> res9: List[Int] = List(1, 1, 2, 2, 3, 3, 4, 4, 5, 5)
nums.length                                       //> res10: Int = 5
nums.last                                         //> res11: Int = 1
nums.init                                         //> res12: List[Int] = List(5, 4, 3, 2)
nums(2)                                           //> res13: Int = 3
nums apply 2                                      //> res14: Int = 3

nums take 2                                       //> res15: List[Int] = List(5, 4)
nums.init drop 2                                  //> res16: List[Int] = List(3, 2)

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