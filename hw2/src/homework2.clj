(ns homework2)

(def all-patients
  '({:firstname "Adam"
     :lastname  "Smith"
     :diagnosis "COVID-19"
     :treated   true}
    {:firstname "Joseph"
     :lastname  "Goodman"
     :diagnosis "COVID-19"
     :treated   true}
    {:firstname "Werner"
     :lastname  "Ziegler"
     :diagnosis "COVID-19"
     :treated   false}
    {:firstname "Boris"
     :lastname  "Henry"
     :diagnosis "Healthy"
     :treated   false}
    {:firstname "Johnny"
     :lastname  "Grayhand"
     :diagnosis "COVID-76"
     :treated   false}))

;(defmacro factor-group [plist gname gmap & body]
;  `(map (fn [~gname] (do ~@body))))

(defn vec-itmes [li] (map (fn [e] (vec [e])) li))

(defn get-variants [maps k]
  (vec-itmes (distinct (map (fn [m] (get m k)) maps))))

(defn find-group [plist ks vs]
  (let [kvs (zipmap ks vs)]
    (filter (fn [amap]
              (reduce (fn [r k]
                        (and r (= (get amap k) (get kvs k))))
                      true ks))
            plist)))

(defn make-groups [plist ks]
  (let [vs (map (partial get-variants plist) ks)]
    (let [combos
          (reduce
           (fn [v1 v2]
             (apply concat (map
                            (fn [b] (map (fn [a] (concat a b)) v1))
                            v2)))
           vs)]
      (let [rcombos (filter (fn [x] (not-empty (find-group plist ks x))) combos)]
        (map (fn [co] (cons (find-group plist ks co) co)) rcombos)))))

(defmacro factor-group [plist gname gmap & body]
  (list 'doall
        (list 'map
              (list 'fn ['x]
                    (list 'apply
                          (list 'fn (into [gname] (filter (comp not keyword?) gmap))
                                (cons 'do body)) 'x))
              (list 'make-groups plist (into [] (filter keyword? gmap))))))

;(print (macroexpand '(factor-group all-patients gname gmap body)))

;(defmacro if-not [cond iftrue iffalse]
;  (list 'if cond iffalse iftrue))
;
; ooops if-not already exists...

(defn decur [foo]
  (fn [& args]
    (let [res (foo (first args))]
      (if-not (fn? res)
        res
        (apply (decur res) (rest args))))))

(defn comp2 [foo buz]
  (fn [& args] (foo (apply buz args))))

(defn compv [foo & funcs]
  (reduce comp2 foo funcs))

(defn juxtv [& funcs]
  (fn [& args] (map (fn [foo] (apply foo args)) funcs)))

;(defn rm-dups [coll]
;  (reduce
;   (fn [c e] (if (contains? c e) c (conj c e)))
;   []
;   coll))
;
; ooooops distinct exists

(defn mergefval [foo maps k]
  (let [vals (map (fn [m] (get m k))
                  (filter (fn [m] (contains? m k)) maps))]
    (if (= 1 (count vals))
      (first vals)
      (apply foo vals))))

(defn mergef [foo & maps]
  (let [rkeys (distinct (apply concat (map keys maps)))]
    (zipmap rkeys (map (partial mergefval foo maps) rkeys))))

; all the "tests" combined
(println
 (= 10 ((decur (fn [a]
                 (fn [b]
                   (fn [c]
                     (fn [d]
                       (+ a b c d))))))
        1 2 3 4))
 (= 24 ((decur (fn [a]
                 (fn [b]
                   (fn [c]
                     (fn [d]
                       (* a b c d))))))
        1 2 3 4))
 (= 25 ((decur (fn [a]
                 (fn [b]
                   (* a b))))
        5 5))
 (= [3 2 1] ((compv rest reverse) [1 2 3 4]))
 (= 5 ((compv (partial + 3) second) [1 2 3 4]))
 (= true ((compv zero? #(mod % 8) +) 3 5 7 9))
 (= "HELLO" ((compv #(.toUpperCase %) #(apply str %) take) 5 "hello world"))
 (= [21 6 1] ((juxtv + max min) 2 3 5 1 6 4))
 (= ["HELLO" 5] ((juxtv #(.toUpperCase %) count) "hello"))
 (= [2 6 4] ((juxtv :a :c :b) {:a 2, :b 4, :c 6, :d 8 :e 10}))
 (= (mergef * {:a 2, :b 3, :c 4} {:a 2} {:b 2} {:c 5})
    {:a 4, :b 6, :c 20})
 (= (mergef - {1 10, 2 20} {1 3, 2 10, 3 15})
    {1 7, 2 10, 3 15})
 (= (mergef concat {:a [3], :b [6]} {:a [4 5], :c [8 9]} {:b [7]})
    {:a [3 4 5], :b [6 7], :c [8 9]}))

(factor-group
 all-patients
 patients-group
 [treated? :treated disease-name :diagnosis]
 (println " начало обработки группы пациентов с диагнозом " disease-name
          (if treated?
            ", подвергавшихся лечению"
            ", НЕ подвергавшихся лечению"))
 (println " количество пациентов в группе - " (count patients-group))
 (println " фамилии пациентов - " (clojure.string/join ", " (map :lastname patients-group)))
 (count patients-group))

