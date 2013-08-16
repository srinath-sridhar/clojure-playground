(ns com.test.arrays.array-stuff)

(def mat-a
  [[1 1 1]
   [1 1 1]
   [1 1 1]
   [1 1 1]])

(def mat-b
  [[1 1 1 1 1]
   [1 1 1 1 1]
   [1 1 1 1 1]])


(defn can-multiply?
  [mat-a mat-b]
  (= (count mat-a) (count (first mat-b))))

(defn is-square?
  [mat]
  (= (count mat) (count (first mat))))

(defn multiply
  [mat-a mat-b]
  (if (can-multiply? mat-a mat-b)
    (let [intermediate
          (map (fn [row-a]
                 (map-indexed (fn [index element]
                                ;; multiply each element with corresponding rows
                                (map #(* element %) (nth mat-b index))
                                ) row-a)) mat-a)]
      (map
       #(map
         (fn [e] (reduce + e)) (invert-matrix %))
       intermediate)
      )))

(defn invert-matrix
  [mat]
  (map
   (fn [index]
     (map #(nth % index) mat))
   (range (count (first mat)))))


#_(clojure.pprint/pprint (multiply mat-a mat-a))

#_(is-square? mat-a)

#_(can-multiply? mat-a mat-a)

#_(invert-matrix mat-a)

#_(def a (map (fn [row-a]
        (map-indexed (fn [index element]
                       ;; multiply each element with corresponding rows
                       (map #(* element %) (nth mat-b index))
                       ) row-a)) mat-a))


#_(map #(map (fn [e] (reduce + e)) (invert-matrix %)) a)

(multiply mat-a (invert-matrix mat-a))

(map #(fn [row]
        (map )) mat-a)
(invert-matrix mat-a)
(defn concise-mul
  [mat-a mat-b])

(defn multiply2
  [mat-a mat-b]
  (let [inv-mat (invert-matrix mat-b)]
    (map (fn [row] (map #(reduce + (map * row  %)) inv-mat)) mat-a)))

(println (multiply2 mat-a mat-b))


(map #(nth %2 %1) (range (count (first mat-a))) mat-a)
