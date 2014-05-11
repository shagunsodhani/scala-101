package functions

import scala.annotation.tailrec

object factorial {

	def fact_notail(n:Int) : Int =
		if(n==1) 1
		else n*fact_notail(n-1)           //> fact_notail: (n: Int)Int
		
	fact_notail(4)                            //> res0: Int = 24

	def fact_tail(n: Int): Int = {
    @tailrec
    def recFac(n: Int, acc: Int): Int =
        if (n == 1) acc
        else recFac(n - 1, acc * n)
    recFac(n, 1)
	}                                         //> fact_tail: (n: Int)Int
	
	fact_tail(5)                              //> res1: Int = 120
}