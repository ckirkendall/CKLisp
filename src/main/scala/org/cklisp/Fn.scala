package org.cklisp
import java.lang.reflect._


abstract class Fn {
  def apply(env: Env, vals: List[Exp]): Any
}

object fn {
  def zipArgs(syms: List[Symbol], vals: List[Exp]): List[(Symbol, Any)] = {
    (syms, vals) match {
      case (Nil,Nil) => Nil
      case (Nil, v::vtail) => throw new RuntimeException("invalid number of args")
      case (s::stail,Nil) => throw new RuntimeException("invalid number of args")
      case (s::stail,v::vtail) => s match {
        case '& => (stail.head,v::vtail)::Nil
        case _ => (s,v)::zipArgs(stail,vtail)
      }
    }
  }
}

class ExpFn(args: List[Symbol], body: List[Exp], origEnv: Env) extends Fn{
  def apply(env: Env, vals: List[Exp]): Any = {
    val nenv = new ChildEnv(origEnv)
    if(!args.isEmpty){
    	val pairs = fn.zipArgs(args,vals);
	    pairs.foreach(pair => pair._2 match {
	      case l: List[Exp] => nenv.assign(pair._1, Handler.handle(l, env))
	      case x: Exp => nenv.assign(pair._1, Handler.handle(x, env))
	      case _ => throw new RuntimeException("invalid arguments")
	    })
    }
    body.map(exp => Handler.handle(exp,nenv)).last
  }  
}

class MacroFn(args: List[Symbol], body: List[Exp], origEnv: Env) extends Fn{
  def apply(env: Env, vals: List[Exp]): Any = {
    val nenv = new ChildEnv(origEnv)
    val pairs = fn.zipArgs(args,vals);
    pairs.foreach(pair => pair._2 match {
      case x: Exp => nenv.assign(pair._1, Helper.unwrap(x))
      case l: List[Exp] => nenv.assign(pair._1, Helper.unwrap(l))
      case _ => throw new RuntimeException("invalid arguments")
    })
    Handler.handle(Helper.wrap(body.map(exp => Handler.handle(exp,nenv)).last), env)
  }  
}


class MethodFn(meth: Symbol) extends Fn {
  def apply(env: Env, vals: List[Exp]): Any = {
    vals match {
      case Nil => throw new RuntimeException("invalid method call: no obj")
      case exp::aexps => 
        val obj = Handler.handle(exp,env)
        val clazz = if(obj.isInstanceOf[java.lang.Class[_]]) obj.asInstanceOf[java.lang.Class[_]] else obj.getClass
        val mname = meth.name.tail
        val fieldOption = if(aexps.isEmpty) clazz.getFields.filter(_.getName.equals(mname)).headOption else None
        fieldOption match {
          case Some(v) => v.get(obj)
          case None =>
            val args = aexps.map((e) => Handler.handle(e,env))
            val rel = ClazzUtils.findBestMatchMethod(clazz, mname, args)
            val m = rel match {
              case EXACT(m) => m
              case COMPATABLE(m) => m
              case NOREL() => throw new RuntimeException("invalid method call")
            }
            m.invoke(obj, args.map(_.asInstanceOf[java.lang.Object]):_*)
        }
        
    }
  }
}

class ConstructorFn(clazz: Symbol) extends Fn {
  def apply(env: Env, vals: List[Exp]): Any = {
    val clazzName=clazz.name.substring(0, clazz.name.length()-1) //remove the last char "."
    vals match {
      case Nil => Class.forName(clazzName).newInstance
      case _ => 
        val args = vals.map((e) => Handler.handle(e,env))
        val rel = ClazzUtils.findBestMatchConstructor(Class.forName(clazzName),args)
        val m = rel match {
          case EXACT(m) => m
          case COMPATABLE(m) => m
          case NOREL() => throw new RuntimeException("invalid method call")
         }
        m.newInstance(args.map(_.asInstanceOf[java.lang.Object]):_*)
    }
  }
}

