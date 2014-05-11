package higher_order_functions

import scala.annotation.tailrec


object PRODUCT {

	def product(f:Int=>Int, a:Int, b:Int) : Int =
		if(a>b) 1
		else f(a)*product(f,a+1,b)        //> product: (f: Int => Int, a: Int, b: Int)Int
		
	def id(x:Int):Int = x                     //> id: (x: Int)Int
	
	def sq(x:Int):Int = x*x                   //> sq: (x: Int)Int
	
	product(id, 1 ,10)                        //> res0: Int = 3628800
	product(sq, 1,3)                          //> res1: Int = 36
	
	def product_tail(f:Int=>Int, a:Int, b:Int) : Int =
	{
		@tailrec
		def product_rec(a:Int, acc:Int):Int =
			if(a>b) acc
			else product_rec(a+1,f(a)*acc)
		
		product_rec(a,1)
	}                                         //> product_tail: (f: Int => Int, a: Int, b: Int)Int
	
	product_tail(id, 1, 10)                   //> res2: Int = 3628800
	product_tail(sq, 1, 3)                    //> res3: Int = 36
	
	
	def fact(n:Int) : Int =
		product((x:Int)=>x,1,n)           //> fact: (n: Int)Int
		
	fact(5)                                   //> res4: Int = 120
	fact(4)                                   //> res5: Int = 24
}