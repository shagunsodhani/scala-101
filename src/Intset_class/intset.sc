package Intset_class

abstract class Intset {
	def incl (x:Int) : Intset
	def contains (x:Int) : Boolean
}