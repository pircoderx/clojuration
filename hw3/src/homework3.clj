(ns homework3)

(def std-env {:+ +, :- -, :* *, :/ /})

(def initial-env {:x 10, :y 20})

(declare custom-eval)

; You can define how functions are stored and called
; by implementing `custom-fn` and `custom-apply`.
; If `custom-fn` returns literaly a function,
; like `fn` does, nothing will broke. 

(defn custom-fn [params body]
  {:params (map keyword params), :body body})

(defn custom-apply [func args env]
  (custom-eval (:body func) (merge env (zipmap (:params func) args))))

(defn custom-eval [expr env]
  (let [env (merge std-env env)]
    (cond
      (symbol? expr)
      (let [val ((keyword expr) env)]
        (if (nil? val)
          (throw (Exception. (str "undefined symbol: " expr)))
          val))
      (list? expr)
      (let [[base arg1 arg2 arg3] expr]
        (cond
          (= base 'defn)
          (assoc env (keyword arg1) (custom-fn arg2 arg3))
          (= base 'fn)
          (custom-fn arg1 arg2)
          :else
          (let [vals (map (fn [subexpr] (custom-eval subexpr env)) expr)
                func (first vals) args (rest vals)]
            (if (fn? func)
              (apply func args)
              (custom-apply func args env)))))
      :else expr)))

(println "Test Arithmetic: "
         (= (custom-eval '(* x y) {:x 10, :y 2}) 20))
(println "Test Nested Expr: "
         (= (custom-eval '(+ x (* y 2)) {:x 5, :y 3}) 11))

(def updated-env
  (custom-eval '(defn sum [a b] (+ a b)) initial-env))

(println "Test Function Call: "
         (= (custom-eval '(sum x y) updated-env) 30))
(println "Test Function Call with Literals: "
         (= (custom-eval '(sum 15 25) updated-env) 40))

(println "Bonus with fn: "
         (= (custom-eval '((fn [a b] (- b a)) 15 25) {}) 10))

(println "Error Handling: "
         (try
           (custom-eval '(sum x z) {:x 1 :y 2})
           (catch Exception e (.getMessage e))))

