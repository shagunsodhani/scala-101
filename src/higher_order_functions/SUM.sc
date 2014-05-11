package higher_order_functions

import scala.annotation.tailrec

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
	
	
	def sum_tail(f:Int=>Int, a:Int, b:Int) : Int =
	{
	
		@tailrec
		def sum_rec(a: Int, acc: Int): Int =
        if (a > b) acc
        else sum_rec(a+1, f(a)+acc)
	
		sum_rec(a,0)
	}                                         //> sum_tail: (f: Int => Int, a: Int, b: Int)Int
	
  sum_tail (id, 1 ,10)                            //> res4: Int = 55
	sum_tail (sq, 1,3)                        //> res5: Int = 14

	//Anonymous function
	sum_tail ((x:Int) => x*x ,1, 3)           //> res6: Int = 14
	sum_tail ( (x:Int) => x*x*x, 1, 3)        //> res7: Int = 36

	
	def	sum_func(f:Int=>Int) : (Int, Int)=>Int = {
		def	sum_f(a:Int, b:Int) : Int =
			if(a>b) 0
			else f(a)+sum_f(a+1,b)
		sum_f
	}                                         //> sum_func: (f: Int => Int)(Int, Int) => Int
	
	sum_func((x:Int) => x*x*x)(1,4)           //> res8: Int = 100
}