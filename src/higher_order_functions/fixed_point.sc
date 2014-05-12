package higher_order_functions

import math.abs

object fixed_point {

	val tolerance = 0.0001                    //> tolerance  : Double = 1.0E-4
	def isCloseEnough(next:Double, guess:Double):Boolean =
		abs( (next-guess)/guess ) < tolerance
                                                  //> isCloseEnough: (next: Double, guess: Double)Boolean
		
	def fixedpoint(f:Double => Double)(firstGuess:Double) : Double = {
	
		def iterate(guess:Double) : Double = {
			val next = f(guess)
			if ( isCloseEnough(next, guess) ) next
			else iterate (next)
		}
		iterate(firstGuess)
	}                                         //> fixedpoint: (f: Double => Double)(firstGuess: Double)Double
	
	fixedpoint(x => x/2+1)(1)                 //> res0: Double = 1.9998779296875
	
}