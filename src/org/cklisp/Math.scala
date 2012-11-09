package org.cklisp
import java.lang.Integer
import java.lang.Double

object Math {
	def plus(x: Integer, y: Integer) = x + y
	def plus(x: Double, y: Integer) = x + y
	def plus(x: Integer, y: Double) = x + y
	def plus(x: Double, y: Double) = x + y
	
	def minus(x: Integer, y: Integer) = x - y
	def minus(x: Double, y: Integer) = x - y
	def minus(x: Integer, y: Double) = x - y
	def minus(x: Double, y: Double) = x - y
	
	def times(x: Integer, y: Integer) = x * y
	def times(x: Double, y: Integer) = x * y
	def times(x: Integer, y: Double) = x * y
	def times(x: Double, y: Double) = x * y
	
	def div(x: Integer, y: Integer) = x / y
	def div(x: Double, y: Integer) = x / y
	def div(x: Integer, y: Double) = x / y
	def div(x: Double, y: Double) = x / y
	
	def less(x: Integer, y: Integer) = x < y
	def less(x: Double, y: Integer) = x < y
	def less(x: Integer, y: Double) = x < y
	def less(x: Double, y: Double) = x < y
	
	def greater(x: Integer, y: Integer) = x > y
	def greater(x: Double, y: Integer) = x > y
	def greater(x: Integer, y: Double) = x > y
	def greater(x: Double, y: Double) = x > y
	
	def lessEq(x: Integer, y: Integer) = x <= y
	def lessEq(x: Double, y: Integer) = x <= y
	def lessEq(x: Integer, y: Double) = x <= y
	def lessEq(x: Double, y: Double) = x <= y
	
	def greaterEq(x: Integer, y: Integer) = x >= y
	def greaterEq(x: Double, y: Integer) = x >= y
	def greaterEq(x: Integer, y: Double) = x >= y
	def greaterEq(x: Double, y: Double) = x >= y
	
	
}