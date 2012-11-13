(println "Welcome to the CKLisp REPL!")

(defn readloop ()
  (print ">")
  (let (exp (readln))
    (println (eval exp)))
  (readloop))

(readloop)