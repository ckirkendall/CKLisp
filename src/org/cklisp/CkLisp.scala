package org.cklisp

object CkLisp extends App {
    val reader = new Reader()
    val env = new ChildEnv(new NilEnv)
    val stdlib = """
      (def true (.TRUE java.lang.Boolean))
      (def false (.FALSE java.lang.Boolean))
      (def cons (fn (item lst) (.$colon$colon lst item)))
      
      (def defmacro (macro (sym & body)
        (let (m (cons 'macro body))
          '(def ~sym ~m))))
      
      (defmacro defn (sym & body) 
        (let (f (cons 'fn body)) 
        	'(def ~sym ~f)))
      
      (defn first (lst) (.head lst))
      (defn rest (lst) (.tail lst))
      
      (defn println (x) (.println (.out java.lang.System) (.toString x)))
      (defn =  (x y) (.equals x y))
      (defn +  (x y) (.plus org.cklisp.Math x y))
      (defn -  (x y) (.minus org.cklisp.Math x y))
      (defn *  (x y) (.times org.cklisp.Math x y))
      (defn >  (x y) (.greater org.cklisp.Math x y))
      (defn <  (x y) (.less org.cklisp.Math x y))
      (defn >= (x y) (.greaterEq org.cklisp.Math x y))
      (defn <= (x y) (.lessEq org.cklisp.Math x y))
    """
      
    val prog = """
      (defn t (x & y) (println x) (println y) (println (cons 3 y)))
      (defmacro m (a b c) '(if ~a ~b ~c))
      (def a 1)
      
      (println "this should print the date and 301")
      (println (java.util.Date.))
      (println (+ 1 (java.lang.Integer. "300")))
      
      (defn b (x) (println x) a)
      
      (let (a 5)
        (println "test1: should print 4 and then 1 if 'macro' and 'defmacro' are working")
        (println (b 4))
        
        (println "test2: should print false if 'if' is working")
        (if false 
          (println "true") 
          (println "false"))
        
        (println "test3: should print 5 if let scope is working")
        (println a) 
      
        (println "test4: should print 3 then List(1, 2.0,1)")
        (println '(1 2.0 ~(b 3))) 
      
        (println "test5: should print 3 then List(4,5) then List(3,4,5)")
        (t 3 4 5) 
      
        (println "test6: should print 5 and get 1 as a final result of 5")
        (m (= a 5) (b (* a 5)) a))
    """
	val pexp =reader.parseAll(reader.namespace, stdlib+prog)
	println(pexp)
	val lexp = pexp.get
	val result=lexp.map(exp => Handler.handle(exp,env)).last
	println("Final Result:"+result)
	
}