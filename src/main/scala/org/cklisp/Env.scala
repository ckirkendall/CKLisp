package org.cklisp

import scala.collection.mutable.HashMap

abstract class Env {
  def assign(sym: Symbol, ex: Any): Unit
  def lookup(sym: Symbol): Any
}

class ChildEnv(parent: Env) extends Env {
  
  val hash = HashMap[Symbol, Any]()
  hash.put(Symbol("*env*"),this)
  
  def assign(sym: Symbol, ex: Any) = {
    hash.put(sym, ex)
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

