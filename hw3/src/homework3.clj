(ns homework3)

(def initial-env {:x 10 :y 20})

(def std-env {:* * :- - :+ +})

(defn sym-kw [sym] (keyword sym))

(defn eval-get [env x]
  (if-not (symbol? x)
    x
    (let [val (get env (sym-kw x))]
      (if (nil? val)
        (throw (Exception. (str "undefined symbol: " x)))
        val))))

(declare my-super-eval)

(defn my-eval [expr env]
  (let [envs (map
              (fn [e] (if (list? e)
                        (my-super-eval e env)
                        {:last-eval (eval-get env e)}))
              expr)]
    (if (empty? envs)
      env
      (let [foo (get (first envs) :last-eval)]
        (if-not (fn? foo)
          (throw (Exception. (str "not a function: " foo)))
          (let [new-env (reduce
                         (fn [e0 e1] (merge e0 (dissoc e1 :last-eval)))
                         {}
                         envs)]
            (assoc
             new-env
             :last-eval
             (apply foo (map
                         (fn [e] (get e :last-eval))
                         (rest envs))))))))))

(defn my-super-eval [expr env]
  (if (empty? expr)
    (assoc env :last-eval ())
    (let [foo (first expr)]
      (if-not (= foo 'defn)
        (my-eval expr env)
        (let [name (nth expr 1) prms (nth expr 2) body (nth expr 3)
              new-fn (fn [& args]
                       (let [fenv
                             (my-super-eval
                              body
                              (if-not (= (count args) (count prms))
                                (throw (Exception. (str "argument mismatch: " name)))
                                (merge (zipmap (map sym-kw prms) args) env)))]
                         (get fenv :last-eval)))]
          (merge
           env
           {(sym-kw name) new-fn
            :last-eval new-fn}))))))

(defn custom-eval [expr env]
  (if-not (and (list? expr) (map? env))
    (throw (Exception. "wrong argument types"))
    (my-super-eval expr (merge std-env env))))

(defn custom-eval-value [expr env]
  (get (custom-eval expr env) :last-eval))

(println "Test Arithmetic: "
         (= (custom-eval-value '(* x y) {:x 10, :y 2}) 20))
(println "Test Nested Expr: "
         (= (custom-eval-value '(+ x (* y 2)) {:x 5, :y 3}) 11))

(def updated-env
  (custom-eval '(defn sum [a b] (+ a b)) initial-env))

(println "Test Function Call: "
         (= (custom-eval-value '(sum x y) updated-env) 30))
(println "Test Function Call with Literals: "
         (= (custom-eval-value '(sum 15 25) updated-env) 40))

(println "Error Handling: "
         (try
           (custom-eval-value '(sum x z) {:x 1 :y 2})
           (catch Exception e (.getMessage e))))

