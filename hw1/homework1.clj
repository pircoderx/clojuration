; homework1.clj by Pirunov Artem

(ns homework1)

(def result (atom {:correct 0 :incorrect 0}))
(defmacro =check [left right]
  `(let [left# ~left
         right# ~right]
     (if (= right# left#)
       (swap! result update :correct inc)
       (do
         (println "missmatch:" ~(str (first left)) left# right#)
         (swap! result update :incorrect inc)))
     left#))

; These functions are not implemented in this work
; but they are used to implement other functions:
; first
; rest
; cons
; conj
; assoc
; dissoc
; get
; -
; +
; =
; or

;; empty? (unused)
(defn my-empty? [a]
  (= (first a) nil)
)
(=check (my-empty? []) true)
(=check (my-empty? [1 2 3]) false)

;; map
(defn my-map [f coll]
  (if (empty? coll)
    []
    (cons (f (first coll)) (my-map f (rest coll)))
  )
)
(=check (my-map inc [1 2 3]) [2 3 4])
(=check (my-map even? [1 2 3]) [false true false])

;; reduce examples
(defn my-reduce [f init coll]
  (if (empty? coll)
    init
    (recur f (f init (first coll)) (rest coll))
  )
)
(=check (my-reduce + 0 [1 2 3 4]) 10)
(=check (my-reduce str "" ["a" "b" "c"]) "abc")
(=check (my-reduce + 0 (range 10000)) 49995000)

;; concat examples
(defn my-concat [coll1 coll2]
  (if (empty? coll2)
    coll1
    (recur (conj coll1 (first coll2)) (rest coll2))
  )
)
(=check (my-concat [1 2] [3 4]) [1 2 3 4])
(=check (count (my-concat (range 5000) (range 5000 10000))) 10000)

;; filter examples
(defn my-filter [pred coll]
  (if (empty? coll)
    []
    (if (pred (first coll))
      (cons (first coll) (my-filter pred (rest coll)))
      (my-filter pred (rest coll))
    )
  )
)
; OR...
(defn my-filter2 [pred coll]
  (if (empty? coll)
    []
    (my-concat
      (if (pred (first coll)) [(first coll)] [])
      (my-filter pred (rest coll))
    )
  )
)
; OR...
(defn my-filter3 [pred coll]
  (my-reduce (fn [a b]
    (my-concat a (if (pred b) [b] []))
  ) [] coll)
)
(=check (my-filter even? [1 2 3 4 5 6]) [2 4 6])
(=check (my-filter #(> (count %) 3) ["hi" "hello" "hey" "greetings"]) ["hello" "greetings"])
(=check (my-filter #(and (even? %) (> % 10)) [12 2 13 14 3]) [12 14])
(=check (my-filter2 even? [1 2 3 4 5 6]) [2 4 6])
(=check (my-filter2 #(> (count %) 3) ["hi" "hello" "hey" "greetings"]) ["hello" "greetings"])
(=check (my-filter2 #(and (even? %) (> % 10)) [12 2 13 14 3]) [12 14])
(=check (my-filter3 even? [1 2 3 4 5 6]) [2 4 6])
(=check (my-filter3 #(> (count %) 3) ["hi" "hello" "hey" "greetings"]) ["hello" "greetings"])
(=check (my-filter3 #(and (even? %) (> % 10)) [12 2 13 14 3]) [12 14])

;; nth examples
(defn my-nth [coll index]
  (if (empty? coll)
    nil
    (if (= index 0)
      (first coll)
      (my-nth (rest coll) (- index 1))
    )
  )
)

(=check (my-nth [10 20 30 40] 2) 30)
(=check (my-nth [1 2 3 4] 10) nil) ; Assuming nil for out of bounds
(=check (my-nth [1 2 3 4] 3) 4)

;; max/min examples
(defn my-best [cmp coll]
  (if (empty? coll)
    nil
    (my-reduce
      (fn [a b] (if (cmp a b) a b))
      (first coll)
      coll
    )
  )
)
(defn my-max [coll] (my-best > coll))
(defn my-min [coll] (my-best < coll))
(=check (my-max [5 3 9 1]) 9)
(=check (my-min [5 3 9 1]) 1)
(=check (my-max [-5 -3 -9 -1]) -1)
(=check (my-min [-5 -3 -9 -1]) -9)
(=check (my-max []) nil)
(=check (my-min []) nil)

;; count examples
(defn my-count [coll]
  (if (empty? coll)
    0
    (+ 1 (my-count (rest coll)))
  )
)
; OR...
(defn my-count2 [coll]
  (my-reduce (fn [a b] (+ a 1)) 0 coll)
)
(=check (my-count [1 2 3 4 5]) 5)
(=check (my-count [[1 2] [3 4] [5]]) 3)
(=check (my-count []) 0)
(=check (my-count2 [1 2 3 4 5]) 5)
(=check (my-count2 [[1 2] [3 4] [5]]) 3)
(=check (my-count2 []) 0)

;; take examples
(defn my-take [n coll]
  (if (empty? coll)
    []
    (if (= n 0)
      []
      (my-concat [(first coll)] (my-take (- n 1) (rest coll)))
    )
  )
)
; OR...
(defn my-range-to-n [n]
  (if (= n 0)
    []
    (conj (my-range-to-n (- n 1)) (- n 1))
  )
)
(defn my-take2 [n coll]
  (my-map (fn [e] (my-nth coll e)) (my-range-to-n n))
)
(=check (my-take 3 [5 4 3 2 1]) [5 4 3])
(=check (my-take2 3 [5 4 3 2 1]) [5 4 3])

;; keys examples
(defn my-keys [dict]
  (if (empty? dict)
    []
    ((fn [k]
      (my-concat [k] (my-keys (dissoc dict k)))
      ; OR (conj (my-keys (dissoc dict k)) k)
      ; but this leads to the worng order in the resulting list
    ) (first (first dict)))
  )
)
(=check (my-keys {:a 1 :b 2 :c 3}) [:a :b :c])
(=check (my-keys {:foo "bar" :baz "qux"}) [:foo :baz])
(=check (my-keys {}) [])

;; merge examples
(defn my-merge [map1 map2]
  (if (empty? map2)
    map1
    (my-reduce
      (fn [a b]
        (assoc a b (get map2 b))
      )
      map1
      (my-keys map2)
    )
  )
)
(=check (my-merge {:a 1 :b 2} {:b 3 :c 4}) {:a 1 :b 3 :c 4})
(=check (my-merge {:foo "bar"} {:foo "baz", :hello "world"}) {:foo "baz", :hello "world"})
(=check (my-merge {} {:a 1}) {:a 1})

;; zipmap
(defn my-zipmap [ks vs]
  (if (or (empty? ks) (empty? vs))
    {}
    (assoc (my-zipmap (rest ks) (rest vs)) (first ks) (first vs))
  )
)
(=check (my-zipmap [:k :b] [1 2]) {:k 1 :b 2})
(=check (my-zipmap [] [1 2]) {})
(=check (my-zipmap [:k :b] []) {})

;; group-by examples
(defn my-group-by [f coll]
  (if (empty? coll)
    {}
    (my-zipmap
      (my-map f coll)
      (my-map
        (fn [e1]
          (my-filter
            (fn [e2] (= (f e2) e1))
            coll
          )
        )
        (my-map f coll)
      )
    )
  )
)
(=check (my-group-by :type [{:type :a :value 1} {:type :b :value 2} {:type :a :value 3}])
        {:a [{:type :a :value 1} {:type :a :value 3}], :b [{:type :b :value 2}]})
(=check (my-group-by even? [1 2 3 4 5 6]) {true [2 4 6], false [1 3 5]})
(=check (my-group-by count ["one" "two" "three" "four"]) {3 ["one" "two"], 5 ["three"], 4 ["four"]})

;; vals examples
(defn my-vals [dict]
  (my-map (fn [e] (get dict e)) (my-keys dict))
)
(=check (my-vals {:a 1 :b 2 :c 3}) [1 2 3])
(=check (my-vals {:foo "bar" :baz "qux"}) ["bar" "qux"])
(=check (my-vals {}) [])

;; select-keys examples
(defn my-select-keys [dict keys-to-select]
  (my-zipmap keys-to-select (my-map (fn [e] (get dict e)) keys-to-select))
)
(=check (my-select-keys {:a 1 :b 2 :c 3} [:a :c]) {:a 1 :c 3})
(=check (my-select-keys {:name "Alice" :age 30 :gender "Female"} [:name :age]) {:name "Alice", :age 30})
(=check (my-select-keys {:foo "bar" :hello "world"} [:foo]) {:foo "bar"})

(println "Test results:" @result)
