(def true (.TRUE java.lang.Boolean))
(def false (.FALSE java.lang.Boolean))
(def cons (fn (item lst) (.$colon$colon lst item)))

(def defmacro (macro (sym & body)
  (let (m (cons 'macro body))
    '(def ~sym ~m))))

(defmacro defn (sym & body) 
  (let (f (cons 'fn body)) 
  	'(def ~sym ~f)))

(defn eval (exp) 
  (let (env *env*
  		reader *reader*
  		pexp (.parseAll reader (.exp reader) exp))
    (.handle org.cklisp.Handler (.get pexp) env)))

(defn first (lst) (.head lst))
(defn rest (lst) (.tail lst))

(defn println (x) (.println (.out java.lang.System) (.toString x)))
(defn print (x) (.print (.out java.lang.System) (.toString x)))
(defn read (x) (.read (.in java.lang.System)))
(defn readln (x) (.readLine (.in java.lang.System)))

(defn =  (x y) (.equals x y))
(defn +  (x y) (.plus org.cklisp.Math x y))
(defn -  (x y) (.minus org.cklisp.Math x y))
(defn *  (x y) (.times org.cklisp.Math x y))
(defn >  (x y) (.greater org.cklisp.Math x y))
(defn <  (x y) (.less org.cklisp.Math x y))
(defn >= (x y) (.greaterEq org.cklisp.Math x y))
(defn <= (x y) (.lessEq org.cklisp.Math x y))