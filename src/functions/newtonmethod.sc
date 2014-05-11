package functions

//Newton's method to calculate square root of a given postive integer

object newtonmethod {
		
	def sqrt(x:Double):Double = {

		def abs(x:Double) : Double =
			if (x<0) -x
			else x
		
		
		def sqrtIter(guess:Double):Double =
			if ( isGoodEnough(guess) ) guess
			else sqrtIter( improve(guess) )
		
		def isGoodEnough(guess:Double):Boolean =
			abs(guess*guess-x) /x <=0.00001
		
		def improve(guess:Double):Double =
			(guess + x/guess)/2
		
		sqrtIter(1)
		
	}                                         //> sqrt: (x: Double)Double
	
	sqrt(1024)                                //> res0: Double = 32.0000071648159

}