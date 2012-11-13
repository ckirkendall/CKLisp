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
  
  (println "testing eval - should print '6'")
  (eval "(println (+ 1 5))")
      
  (println "test6: should print 5 and get 1 as a final result of 5")
  (m (= a 5) (b (* a 5)) a))