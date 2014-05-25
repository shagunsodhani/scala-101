import List._

object nth {
	
	def nth[T](n:Int, xs:List[T]): T =
		if(xs.isEmpty) throw new IndexOutOfBoundsException
 		else if (n==0) xs.head
		else nth(n-1, xs.tail)            //> nth: [T](n: Int, xs: List.List[T])T

	val list = new Cons (1, new Cons(2, new Cons(3, new Nil) ) )
                                                  //> list  : List.Cons[Int] = List.Cons@9c3ed1
	nth(2, list)                              //> res0: Int = 3

}