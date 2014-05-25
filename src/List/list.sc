package List

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