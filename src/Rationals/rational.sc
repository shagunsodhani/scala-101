package Rationals

class Rational(x:Int, y:Int){
	def num = x
	def den = y
}

object rational {
	
	def a = new Rational(1, 2)                //> a: => Rationals.Rational
	a.num + a.den                             //> res0: Int = 3
	a.den                                     //> res1: Int = 2
	
	def b = new Rational(1,4)                 //> b: => Rationals.Rational
	
	def addrational(r:Rational, s:Rational) : Rational = {
		new Rational( r.num*s.den + r.den*s.num, r.den*s.den)
	}                                         //> addrational: (r: Rationals.Rational, s: Rationals.Rational)Rationals.Rationa
                                                  //| l
  def mulrational(r:Rational, s:Rational) : Rational = {
		new Rational( r.num*s.num, r.den*s.den)
	}                                         //> mulrational: (r: Rationals.Rational, s: Rationals.Rational)Rationals.Rationa
                                                  //| l
  def negrational(r:Rational) : Rational = {
  	new Rational(-r.num, r.den)
  }                                               //> negrational: (r: Rationals.Rational)Rationals.Rational
  
  def invrational(r:Rational) : Rational = {
  	new Rational(r.den, r.num)
  }                                               //> invrational: (r: Rationals.Rational)Rationals.Rational
  
  
  def makeString(r:Rational) =
  	r.num + "/" + r.den                       //> makeString: (r: Rationals.Rational)String
  	
  makeString(addrational(a,b))                    //> res2: String = 6/8
  
  makeString(addrational(negrational(a),b))       //> res3: String = -2/8
  
  makeString(mulrational(invrational(a),b))       //> res4: String = 2/4
  
  
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
}