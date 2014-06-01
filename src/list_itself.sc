val fruit: List[String] = List("apples", "oranges", "pears")
                                                  //> fruit  : List[String] = List(apples, oranges, pears)

fruit.head                                        //> res0: String = apples

fruit.tail.head                                   //> res1: String = oranges

// List of Integers
val nums: List[Int] = List(1, 2, 3, 4)            //> nums  : List[Int] = List(1, 2, 3, 4)

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
