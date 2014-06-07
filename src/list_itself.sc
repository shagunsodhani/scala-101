import math.Ordering

object list_itself {

val temp : List[Int] = List(1, 1, 3, 2, 3, 2)     //> temp  : List[Int] = List(1, 1, 3, 2, 3, 2)

def pack[T](xs: List[T]): List[List[T]] = xs match
{
	case Nil => Nil
	case x :: xs1 =>
		{
			val (first, rest) = (xs filter (y => y==x), xs filterNot( y => y ==x))
			first::pack(rest)
		}
}                                                 //> pack: [T](xs: List[T])List[List[T]]

pack(temp)                                        //> res0: List[List[Int]] = List(List(1, 1), List(3, 3), List(2, 2))

val nums: List[Int] = List(5, 4, 3, 2, 1)         //> nums  : List[Int] = List(5, 4, 3, 2, 1)

val pair = ("shagun", 14.04)                      //> pair  : (String, Double) = (shagun,14.04)
val (name, year) = pair                           //> name  : String = shagun
                                                  //| year  : Double = 14.04
pair._1                                           //> res1: String = shagun

val bigpair = ("shagun", "sodhani", "trusty", 14.04)
                                                  //> bigpair  : (String, String, String, Double) = (shagun,sodhani,trusty,14.04)
val biggerpair = (pair._1, pair._2, pair, "trusty", 14.04)
                                                  //> biggerpair  : (String, Double, (String, Double), String, Double) = (shagun,1
                                                  //| 4.04,(shagun,14.04),trusty,14.04)
biggerpair._1                                     //> res2: String = shagun


val fruit: List[String] = List("apples", "oranges", "pears", "banana")
                                                  //> fruit  : List[String] = List(apples, oranges, pears, banana)
fruit.head                                        //> res3: String = apples
fruit.tail.head                                   //> res4: String = oranges

// List of Integers
//val nums: List[Int] = List(5, 4, 3, 2, 1)

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

def msort[T] (xs : List[T] )(lt : (T, T) => Boolean) : List[T] = {
	val n = xs.length/2
	if (n==0) xs
	else {
		/*def merge(xs : List[Int], ys : List[Int] ): List[Int] = xs match {
			case List() => ys
			case x :: xs1 => ys match {
				case List() => xs
				case y :: ys1 =>
					if (x < y) x :: merge(xs1, ys)
					else y :: merge (xs, ys1)
			}
		}*/
		def merge(xs : List[T], ys : List[T]) : List[T] = (xs, ys) match {
			case (Nil, ys) => ys
			case (xs, Nil) => xs
			case (x :: xs1, y :: ys1) =>
				if (lt(x, y)) x :: merge(xs1, ys)
				else y :: merge(xs, ys1)
		}
		val (fst, snd) = xs splitAt n
		merge(msort(fst)(lt), msort(snd)(lt))
	}
}                                                 //> msort: [T](xs: List[T])(lt: (T, T) => Boolean)List[T]


def scaleList(xs: List[Int], factor : Int) : List[Int] =
	xs map (x => x*factor)                    //> scaleList: (xs: List[Int], factor: Int)List[Int]

scaleList(nums, 2)                                //> res5: List[Int] = List(10, 8, 6, 4, 2)

def squareList(xs: List[Int]): List[Int] = xs match {
	case Nil => Nil
	case y :: ys => y*y :: squareList(ys)
}                                                 //> squareList: (xs: List[Int])List[Int]

def squareList2(xs: List[Int]): List[Int] =
	xs map (x => x*x)                         //> squareList2: (xs: List[Int])List[Int]

def posElem(xs : List[Int]) : List[Int] = xs match {
	case Nil => Nil
	case y :: ys => if (y>0) y :: posElem(ys) else posElem (ys)
}                                                 //> posElem: (xs: List[Int])List[Int]

def posElems2(xs: List[Int]): List[Int] =
	xs filter (x => x > 0)                    //> posElems2: (xs: List[Int])List[Int]


nums filter (x => x > 2)                          //> res6: List[Int] = List(5, 4, 3)
nums filterNot (x => x > 2)                       //> res7: List[Int] = List(2, 1)
nums partition (x=> x > 2)                        //> res8: (List[Int], List[Int]) = (List(5, 4, 3),List(2, 1))
nums takeWhile (x=> x > 2)                        //> res9: List[Int] = List(5, 4, 3)
nums dropWhile (x=> x > 2)                        //> res10: List[Int] = List(2, 1)
nums span (x=> x > 2)                             //> res11: (List[Int], List[Int]) = (List(5, 4, 3),List(2, 1))

nums                                              //> res12: List[Int] = List(5, 4, 3, 2, 1)
msort(nums)((x: Int, y : Int) => x < y)           //> res13: List[Int] = List(1, 2, 3, 4, 5)

msort(fruit)((x: String, y: String) => x.compareTo(y) < 0)
                                                  //> res14: List[String] = List(apples, banana, oranges, pears)

def removeAt[T](xs: List[T], n: Int) : List[T] = (xs take n) ::: (xs drop n+1)
                                                  //> removeAt: [T](xs: List[T], n: Int)List[T]


def msortord[T] (xs : List[T] )(implicit ord: Ordering[T]) : List[T] = {
	val n = xs.length/2
	if (n==0) xs
	else {
		/*def merge(xs : List[Int], ys : List[Int] ): List[Int] = xs match {
			case List() => ys
			case x :: xs1 => ys match {
				case List() => xs
				case y :: ys1 =>
					if (x < y) x :: merge(xs1, ys)
					else y :: merge (xs, ys1)
			}
		}*/
		def merge(xs : List[T], ys : List[T]) : List[T] = (xs, ys) match {
			case (Nil, ys) => ys
			case (xs, Nil) => xs
			case (x :: xs1, y :: ys1) =>
				if (ord.lt(x, y)) x :: merge(xs1, ys)
				else y :: merge(xs, ys1)
		}
		val (fst, snd) = xs splitAt n
		merge(msortord(fst)(ord), msortord(snd)(ord))
	}
}                                                 //> msortord: [T](xs: List[T])(implicit ord: scala.math.Ordering[T])List[T]

msortord(fruit)//(Ordering.String)                //> res15: List[String] = List(apples, banana, oranges, pears)

removeAt(sortnum, 1)                              //> res16: List[Int] = List(1, 3, 4, 5)
sortnum                                           //> res17: List[Int] = List(1, 2, 3, 4, 5)
reverse (sortnum)                                 //> res18: List[Int] = List(5, 4, 3, 2, 1)

concat(sortnum, nums)                             //> res19: List[Int] = List(1, 2, 3, 4, 5, 5, 4, 3, 2, 1)
init(sortnum)                                     //> res20: List[Int] = List(1, 2, 3, 4)

last(sortnum)                                     //> res21: Int = 5
sortnum                                           //> res22: List[Int] = List(1, 2, 3, 4, 5)
sortnum updated (2,2)                             //> res23: List[Int] = List(1, 2, 2, 4, 5)

sortnum.contains(5)                               //> res24: Boolean = true
sortnum contains 25                               //> res25: Boolean = false
sortnum indexOf 25                                //> res26: Int = -1

isort(sortnum++nums.reverse)                      //> res27: List[Int] = List(1, 1, 2, 2, 3, 3, 4, 4, 5, 5)
nums.length                                       //> res28: Int = 5
nums.last                                         //> res29: Int = 1
nums.init                                         //> res30: List[Int] = List(5, 4, 3, 2)
nums(2)                                           //> res31: Int = 3
nums apply 2                                      //> res32: Int = 3

nums take 2                                       //> res33: List[Int] = List(5, 4)
nums.init drop 2                                  //> res34: List[Int] = List(3, 2)

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
  
}