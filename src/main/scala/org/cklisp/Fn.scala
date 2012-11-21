package org.cklisp
import java.lang.reflect._


abstract class Fn {
  def apply(env: Env, vals: List[Exp]): Any
}

object Fn {
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
    	val pairs = Fn.zipArgs(args,vals);
	    pairs.foreach(pair => pair._2 match {
	      case l: List[Exp] => nenv.assign(pair._1, Handler.handle(l, env))
	      case x: Exp => nenv.assign(pair._1, Handler.handle(x, env,true))
	      case _ => throw new RuntimeException("invalid arguments")
	    })
    }
    new Thunk(body,nenv)
  }  
}

class MacroFn(args: List[Symbol], body: List[Exp], origEnv: Env) extends Fn{
  def apply(env: Env, vals: List[Exp]): Any = {
    val nenv = new ChildEnv(origEnv)
    val pairs = Fn.zipArgs(args,vals);
    pairs.foreach(pair => pair._2 match {
      case x: Exp => nenv.assign(pair._1, Helper.unwrap(x))
      case l: List[Exp] => nenv.assign(pair._1, Helper.unwrap(l))
      case _ => throw new RuntimeException("invalid arguments")
    })
    new Thunk(List(Helper.wrap(body.map(exp => Handler.handle(exp,nenv,true)).last)), env)
  }  
}


class MethodFn(meth: Symbol) extends Fn {
  def apply(env: Env, vals: List[Exp]): Any = {
    try { 
	    vals match {
	      case Nil => throw new RuntimeException("invalid method call: no obj")
	      case exp::aexps => 
	        val obj = Handler.handle(exp,env,true)
	        val clazz = if(obj.isInstanceOf[java.lang.Class[_]]) obj.asInstanceOf[java.lang.Class[_]] else obj.getClass
	        val mname = meth.name.tail
	        val fieldOption = if(aexps.isEmpty) clazz.getFields.filter(_.getName.equals(mname)).headOption else None
	        fieldOption match {
	          case Some(v) => v.get(obj)
	          case None =>
	            val args = aexps.map((e) => Handler.handle(e,env,true))
	            val rel = ClazzUtils.findBestMatchMethod(clazz, mname, args)
	            val m = rel match {
	              case EXACT(m) => m
	              case COMPATABLE(m) => m
	              case NOREL() => throw new RuntimeException("invalid method call:"+clazz+":"+obj+":"+meth)
	            }
	            m.invoke(obj, args.map(_.asInstanceOf[java.lang.Object]):_*)
	        }
	    }
	 } catch {
        case error: Throwable => {
          println("invalid method call:"+meth)
          throw error
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
        val args = vals.map((e) => Handler.handle(e,env,true))
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


class tryFn extends Fn {
  def isCatch(exp: Exp) = exp match {
      case LIST(v) => v match {
        case xs::tail => xs match {
          case SYMBOL(Symbol("catch")) => true
          case _ => false
        }
        case _ => false
      }
    }
  def catchTransform(cExp: LIST): (Class[_], Exp) = cExp.value match {
    case _::arg::body => arg match {
      case LIST(SYMBOL(x)::SYMBOL(y)::Nil) => (Class.forName(y.name),LIST(List('let,List(x,Symbol("*ex*"))).map(Helper.wrap):::body))
      case _ => throw new RuntimeException("invalid argument to catch statement")
    }
    case _ => throw new RuntimeException("invalid catch statement")
  }
  
  def apply(env: Env, vals: List[Exp]): Any = {
    val body = vals.filter((a) => !isCatch(a))
    val catches = vals.filter((a) => isCatch(a)).map((a) => catchTransform(a.asInstanceOf[LIST]))
    try{
      Handler.handle(body,env).last
    }catch {
      case error: java.lang.Throwable => catches.dropWhile((exp) => !exp._1.isAssignableFrom(error.getClass())) match {
        case Nil => throw error
        case head::tail => {
          env.assign(Symbol("*ex*"), error)
          Handler.handle(head._2,env,true)
          error.printStackTrace()
          "Error"
        }
      }
    }
    
  }
}


class Thunk(body: List[Exp], env: Env){
  def apply(): Any = {
    Handler.handleWithTailCall(body, env).last
  }
}