package org.cklisp

import scala.collection.mutable.HashMap

abstract class Env {
  def assign(sym: Symbol, ex: Any): Unit
  def lookup(sym: Symbol): Any
}

class ChildEnv(parent: Env) extends Env {
  
  val hash = HashMap[Symbol, Any]()
  
  def assign(sym: Symbol, ex: Any) = {
    //println("assigning:" + sym + " to " + ex)
    hash.put(sym, ex)
    //println("map: " + hash)
    //println(hash.get(sym))
  }
  
  def lookup(sym: Symbol) : Any = {
    hash.get(sym).getOrElse(parent.lookup(sym))
  }
  
  override def toString: String = {
    "Current Scope:" + hash.toString() + " ParentScope:" + parent.toString() 
  }
}

class NilEnv extends Env {
  def assign(sym: Symbol, ex: Any): Unit = Unit
  def lookup(sym: Symbol): Any = throw new RuntimeException("invalid reference:"+sym)
}

