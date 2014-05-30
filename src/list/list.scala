package list

object list {

  // List of Strings
val fruit: List[String] = List("apples", "oranges", "pears")

// List of Integers
val nums: List[Int] = List(1, 2, 3, 4)

// Empty List.
val empty: List[Nothing] = List()

val fruits = "apples" :: ("oranges" :: ("pears" :: Nil))

val num = ( 1 ::  (2 :: (3 :: Nil) ) )

val num1 = 1 :: 2 :: 3 :: Nil 
  
val nulla = empty

val num2 = Nil.::(4).::(3).::(2).::(1)

}