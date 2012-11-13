(println "Welcome to the CKLisp REPL!")

(defn seval (exp) 
  (let (env *coreenv*
        reader *reader*
        pexp (.parseAll reader (.exp reader) exp))
    (.handle org.cklisp.Handler (.get pexp) env)))

(defn readloop ()
  (print "cklisp>")
  (let (exp (readln))
    (println (seval exp)))
  (readloop))

(readloop)