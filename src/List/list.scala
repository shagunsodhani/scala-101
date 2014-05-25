package List

object list{

	def singleton[T](elem: T) : List[T]= new Cons[T](elem, new Nil[T])
                                                  //> singleton: [T](elem: T)List.List[T]
	singleton[Int](1)                         //> res0: List.List[Int] = List.Cons@1c7f55b
	singleton[Boolean](true)                  //> res1: List.List[Boolean] = List.Cons@bd327b

}


trait List[T]{
	def isEmpty : Boolean
	def head : T
	def tail : List[T]
}

class Cons[T](val head : T, val tail : List[T]) extends List[T] {
	def isEmpty : Boolean = false
}

class Nil[T] extends List[T] {
	def isEmpty : Boolean = true
	def head : T = throw new NoSuchElementException("Nil.head")
	def tail : List[T] = throw new NoSuchElementException("Nil.tail")
}

//def singleton[T](elem : T) : List[T]= new Cons[T](elem, new Nil[T])