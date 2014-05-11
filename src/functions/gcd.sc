package functions

import scala.annotation.tailrec

object gcd {

def callgcd(a:Int, b:Int) = {
  @tailrec
  def gcd(a:Int, b:Int):Int =
        if (b==0) a else gcd(b, a % b)
  gcd(a,b)
 }                                                //> callgcd: (a: Int, b: Int)Int
 callgcd(1,2)                                     //> res0: Int = 1
 callgcd(5,25)                                    //> res1: Int = 5
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
}