package higher_order_functions

//implementation of higher order functions buil around sum function

object SUM {

	def sum(f:Int=>Int, a:Int, b:Int) : Int =
		if(a>b) 0
		else f(a) + sum(f,a+1,b)          //> sum: (f: Int => Int, a: Int, b: Int)Int
		
	def id(x:Int):Int = x                     //> id: (x: Int)Int
	
	def sq(x:Int):Int = x*x                   //> sq: (x: Int)Int
	
	sum (id, 1 ,10)                           //> res0: Int = 55
	sum (sq, 1,3)                             //> res1: Int = 14

	//Anonymous function
	sum ((x:Int) => x*x ,1, 3)                //> res2: Int = 14
	sum ( (x:Int) => x*x*x, 1, 3)             //> res3: Int = 36
	
  
}