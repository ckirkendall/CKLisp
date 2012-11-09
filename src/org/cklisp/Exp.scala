package org.cklisp

abstract class Exp

case class STRING(value: String) extends Exp
case class INUM(value : Int) extends Exp
case class FNUM(value : Double) extends Exp
case class SYMBOL(value: Symbol) extends Exp
case class BOOL(value: Boolean) extends Exp
case class LIST(value: List[Exp]) extends Exp
case class QUOTE(value: Exp) extends Exp
case class UNQUOTE(value: Exp) extends Exp



