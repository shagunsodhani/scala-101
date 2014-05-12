package higher_order_functions

object OPERATOR {
	
	def operate (f : Int => Int, combine : (Int, Int) => Int, zero : Int)(a:Int, b:Int) : Int =
		if(a>b) zero
		else combine( f(a), operate(f,combine,zero)(a+1,b) )
                                                  //> operate: (f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: I
                                                  //| nt)Int
	
	def product(f : Int => Int)(a : Int, b : Int) : Int = operate(f, (x,y)=>x*y, 1)(a, b)
                                                  //> product: (f: Int => Int)(a: Int, b: Int)Int
	
	product((x:Int)=>x)(1,6)                  //> res0: Int = 720
	
	def sum(f : Int => Int)(a : Int, b : Int) : Int = operate(f, (x,y)=>x+y, 0)(a, b)
                                                  //> sum: (f: Int => Int)(a: Int, b: Int)Int
	sum((x:Int)=>x)(1,10)                     //> res1: Int = 55
	
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
}