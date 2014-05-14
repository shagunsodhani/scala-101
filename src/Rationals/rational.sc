package Rationals

class Rational(x:Int, y:Int){

	require(y!=0, "Denominator must be non negative")
	
	def this(x:Int) = this(x,1)
	   
	private def gcd (a:Int, b:Int) : Int=
		if(b==0) a
		else gcd(b, a%b)
	
	private val g = gcd(x, y)
	
	def num = x/g
	def den = y/g
	 		
	/*
	def add(s:Rational) : Rational = {
		new Rational(num*s.den + den*s.num, den*s.den)
	}
	
	def mul(s:Rational) : Rational = {
		new Rational(num*s.num, den*s.den)
	}
	
	def neg : Rational =
		new Rational(-num,den)
		
	def inv : Rational =
		new Rational(den,num)
		
	def sub(s:Rational) : Rational = {
		add(s.neg)
	}
	
	def div(s:Rational) : Rational = {
		mul(s.inv)
	}
	
	def less(s:Rational) : Boolean =
		num*s.den < den*s.num
	
	def max(s:Rational) : Rational =
		if ( this.less(s) ) s
		else this
		
	*/
	
	def + (s:Rational) : Rational = {
		new Rational(num*s.den + den*s.num, den*s.den)
	}
	
	def * (s:Rational) : Rational = {
		new Rational(num*s.num, den*s.den)
	}
	
	def unary_- : Rational =
		new Rational(-num,den)
		
	def inv : Rational =
		new Rational(den,num)
		
	def - (s:Rational) : Rational = {
		this + -s
	}
	
	def / (s:Rational) : Rational = {
		this * s.inv
	}
	
	def < (s:Rational) : Boolean =
		num*s.den < den*s.num
	
	def max(s:Rational) : Rational =
		if ( this < s ) s
		else this
		
	override def toString =
  	num + "/" + den
	
}

object rational {
	
	def a = new Rational(1, 2)                //> a: => Rationals.Rational
	a.num + a.den                             //> res0: Int = 3
	a.den                                     //> res1: Int = 2
	
	def b = new Rational(2,1)                 //> b: => Rationals.Rational
	
	def addrational(r:Rational, s:Rational) : Rational = {
		new Rational( r.num*s.den + r.den*s.num, r.den*s.den)
	}                                         //> addrational: (r: Rationals.Rational, s: Rationals.Rational)Rationals.Ration
                                                  //| al
  def mulrational(r:Rational, s:Rational) : Rational = {
		new Rational( r.num*s.num, r.den*s.den)
	}                                         //> mulrational: (r: Rationals.Rational, s: Rationals.Rational)Rationals.Ration
                                                  //| al
  def negrational(r:Rational) : Rational = {
  	new Rational(-r.num, r.den)
  }                                               //> negrational: (r: Rationals.Rational)Rationals.Rational
  
  def invrational(r:Rational) : Rational = {
  	new Rational(r.den, r.num)
 	}                                         //> invrational: (r: Rationals.Rational)Rationals.Rational
  
  
  def makeString(r:Rational) =
  	r.num + "/" + r.den                       //> makeString: (r: Rationals.Rational)String
  	
  makeString(addrational(a,b))                    //> res2: String = 5/2
  
  a + b                                           //> res3: Rationals.Rational = 5/2
  a - b                                           //> res4: Rationals.Rational = 3/-2
  a*b                                             //> res5: Rationals.Rational = 1/1
  a/b                                             //> res6: Rationals.Rational = 1/4
  a max b                                         //> res7: Rationals.Rational = 2/1
  a < b                                           //> res8: Boolean = true
  makeString(addrational(negrational(a),b))       //> res9: String = 3/2
  
  makeString(mulrational(invrational(a),b))       //> res10: String = 4/1
 	
 	a.max(b)                                  //> res11: Rationals.Rational = 2/1
  
  a.max(new Rational(4))                          //> res12: Rationals.Rational = 4/1
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
}