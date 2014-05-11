package functions

//Newton's method to calculate square root of a given postive integer

object newtonmethod {

	def sqrtIter(guess:Double, x:Double):Double =
		if ( isGoodEnough(guess,x) ) guess
		else sqrtIter(improve(guess,x),x) //> sqrtIter: (guess: Double, x: Double)Double
		
	def isGoodEnough(guess:Double, x:Double):Boolean =
		abs(guess*guess-x) /x <=0.00001   //> isGoodEnough: (guess: Double, x: Double)Boolean
		
	def improve(guess:Double, x:Double):Double =
		(guess + x/guess)/2               //> improve: (guess: Double, x: Double)Double

	def abs(y:Double):Double =
		if (y < 0) -y
		else y                            //> abs: (y: Double)Double
		
	def sqrt(x:Double):Double = sqrtIter(1,x) //> sqrt: (x: Double)Double
	
	sqrt(4)                                   //> res0: Double = 2.0000000929222947

}