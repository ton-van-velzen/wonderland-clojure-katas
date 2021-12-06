(ns wonderland-number.finder)
  "Wonderland is a strange place.  There is a wonderland number that is also quite strange.

   ![White Rabbit](/images/whiterabbit.gif)

   You must find a way to generate this wonderland number.

   - It has six digits
   - If you multiply it by 2,3,4,5, or 6, the resulting number has all
     the same digits in at as the original number.  The only difference
     is the position that they are in. 
  "
(defn digits [n]
  "returns seq of digits in integer argument"
  (->> n
       (iterate #(quot % 10))
       (take-while pos?)
       (mapv #(mod % 10))
       rseq))

(defn to-digits-set [number]
  "returns ordered set of unique digits in integer argument"
  (apply sorted-set (digits number)))

(defn has-six-digits? [number]
   (= 6 (count (digits number))))

(defn products [n]
  (map (comp to-digits-set #(* n %)) [2 3 4 5 6]))

(defn match? [num elt coll]
  (and (has-six-digits? num)
       (every? true? (map #(= elt %) coll))))

(defn p-prod [n]
  (println n "->"  (* n 2)  (* n 3) (* n 4) (* n 5) (* n 6)))

(defn wonderland-number? [number]
  "returns predicate of two transformations of integer argument"
  (match? number (to-digits-set number) (products number)))

(defn wonderland-number
  ([]  ; without argument return the first 6 digit number with wonderland properties
   (first (filter wonderland-number? (range 100000 1000000))))
  ([n] ; given a 6-digit numeric argument we print and return it when it is a wonderland number
   (if (not (has-six-digits? n))
     (println "Please, enter a six digit number to test for the wonderland property")
     (if (wonderland-number?  n)
       (do
         (p-prod n)
         n)))))