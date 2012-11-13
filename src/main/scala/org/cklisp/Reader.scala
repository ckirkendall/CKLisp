package org.cklisp

import scala.util.parsing.combinator.RegexParsers

class Reader extends RegexParsers{ 
 
  def ident: Parser[SYMBOL] = """[a-zA-Z_<>=?\.+\-/\*][a-zA-Z_<>=?\$\.\*]*""".r ^^ { id => SYMBOL(Symbol(id)) }
  
  def amp: Parser[SYMBOL] = """&""".r ^^ { sym => SYMBOL(Symbol(sym)) }
  
  def str: Parser[STRING] = "\"" ~> """([^"\p{Cntrl}\\]|\\[\\/bfnrt]|\\u[a-fA-F0-9]{4})*""".r <~ "\"" ^^ {  str => STRING(str) }
  
  def inum: Parser[INUM] = """-?\d+""".r ^^ { num => INUM(num.mkString.toInt) }
  
  def fnum: Parser[FNUM] = """-?\d*\.\d+""".r ^^ { num => FNUM(num.mkString.toDouble) }
  
  def lst: Parser[LIST] = "(" ~> repsep(exp, """\s*""".r) <~ """\)\w*""".r ^^ { args => LIST(args) }
  
  def quote: Parser[QUOTE] = "'" ~> act ^^ { obj => QUOTE(obj) }
  
  def unquote: Parser[UNQUOTE] = "~" ~> act ^^ { obj => UNQUOTE(obj) }
  
  def act = ident | lst 
  
  def exp = str | fnum | inum | lst | ident | quote | unquote | amp
  
  def namespace = rep(exp)
}