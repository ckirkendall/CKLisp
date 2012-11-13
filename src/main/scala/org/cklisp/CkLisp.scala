package org.cklisp
import scala.io.Source

object CkLisp extends App {
  
  def handleFile(reader: Reader, env: Env, file: String): Any = {
    val prog = loadFile(file)
    val exps =reader.parseAll(reader.namespace, prog)
    exps.get.map(exp => Handler.handle(exp,env)).last
  }
  
  def loadFile(path: String): String = {
    val classLoader = Thread.currentThread.getContextClassLoader()
    val fileStream = classLoader.getResourceAsStream(path)
    val source = Source.fromInputStream(fileStream)
    val lib=source.mkString
    source.close
    println(lib)
    lib
  }
  
  override def main(args: Array[String]) {
    val reader = new Reader()
  
    val env = new ChildEnv(new NilEnv)
    env.assign(Symbol("*reader*"), reader)
    env.assign(Symbol("*coreenv*"), env)
    val coreLib=loadFile("org/cklisp/core.lisp")   
    
    val coreExp =reader.parseAll(reader.namespace, coreLib)
    coreExp.get.map(exp => Handler.handle(exp,env))
    println(coreExp)
    args.toList match {
      case Nil => handleFile(reader,env, "org/cklisp/repl.lisp")
      case "-f"::file::rest => println("Final Result:"+handleFile(reader,env, file))
      case "--test"::rest => println("Final Result:"+handleFile(reader,env, "org/cklisp/test.lisp"))
      case _ => {
          println("USAGE:")
    	  println("no args => REPL")
    	  println("-f <file> => run file")
    	  println("--test => run test file")
      }
    }
    
    
    
    
   }  
    
	
}