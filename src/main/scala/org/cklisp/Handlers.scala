package org.cklisp

object Handler {
	def handle(exp: Exp, env: Env): Any = exp match {
	    case INUM(v) => v
	    case FNUM(v) => v
	    case STRING(v) => v
	    case QUOTE(v) => Helper.unwrap(v,true,env)
	    case UNQUOTE(v) => throw new RuntimeException("invalid unquote")
	    case SYMBOL(v) => handleSymbol(v,env)
	    case LIST(v) => callProc(v,env)
	  }
	
	def handle(list: List[Exp], env: Env): List[Any] = list match {
	  case Nil => Nil
	  case xs::tail => handle(xs,env)::handle(tail,env)
	}
		
	def callProc(obj: List[Exp], env: Env) = obj match {
	  case Nil => Nil
	  case xs::tail => xs match {
	      case SYMBOL(sym) => sym match {
	        case 'macro => handleMacro(tail,env)
	        case 'def => handleDef(tail,env)
	        case 'let => handleLet(tail, env)
	        case 'fn => handleFn(tail,env)
	        case 'if => handleIf(tail,env)
	        case _ => handle(xs,env) match {
	          case fn: Fn => fn(env, tail)
	          case _ => throw new RuntimeException("not a function:" + sym)
	        }
	      }
	      case _ => handle(xs,env) match {
	        case fn: Fn => fn(env,tail)
	        case _ => throw new RuntimeException("invlid function call")
	      }
	    }
	}
	
	def handleFn(exps: List[Exp], env: Env): Fn = exps match {
	    case Nil => throw new RuntimeException("invalid fn statement")
	    case xs::tail => Helper.unwrap(xs) match {
	      case Nil => new ExpFn(Nil,tail,env)
	      case args: List[Any] => new ExpFn(Helper.listAnytoListSymbol(args),tail,env) 
	      case _ => throw new RuntimeException("invalid fn statement")
	    }
	  }
  
  def handleMacro(exp: List[Exp], env: Env): Fn = exp match {
	    case Nil => throw new RuntimeException("invalid macro statement")
	    case xs::tail => Helper.unwrap(xs) match {
	      case args: List[Any] => new MacroFn(Helper.listAnytoListSymbol(args),tail,env) 
	      case _ => throw new RuntimeException("invalid macro statement")
	    }
	  }
	
  def handleDef(obj: List[Exp], env: Env) = obj match {
	  case Nil => throw new RuntimeException("invalid let statement")
	  case xs::tail => Helper.unwrap(xs) match {
	    case sym: Symbol => env.assign(sym, handle(tail.head, env))
	    case _ => throw new RuntimeException("invalid let statement")
	  }
  }
  
  def handleLet(exps: List[Exp], env: Env): Any = exps match {
    case Nil => throw new RuntimeException("invalid let statement")
    case xs::body => xs match {
      case assign: LIST => {
        val pairs = Helper.buildAssignmentList(assign.value)
        val nenv = new ChildEnv(env)
        pairs.foreach(pair => nenv.assign(pair._1, handle(pair._2,nenv)))
        body.map(exp => handle(exp,nenv)).last
      }
      case _ => throw new RuntimeException("invalid let statement")
    }
  }
	
  def handleIf(exp: List[Exp], env: Env): Any = exp match {
    case test::texp::fexp::tail => {
      val tval = handle(test,env)
      val bool = tval match { 
        case null => false
        case false => false
        case _ => true
      }
      if(bool) handle(texp,env) else handle(fexp, env)
    }
    case _ => throw new RuntimeException("invalid if statement")
  }
  
  def handleSymbol(exp: Symbol, env: Env): Any = {
    if(exp.name.startsWith(".")) new MethodFn(exp)
    else if (exp.name.endsWith(".")) new ConstructorFn(exp)
    else if (exp.name.contains(".")) Class.forName(exp.name)
    else env.lookup(exp)
  }
	
}