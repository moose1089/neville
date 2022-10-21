(ns neville.core
  (:require [clojure.tools.trace :as trace])
  (:gen-class))

;; Polynomials represented by coefficients in reverse order
;; [1 2 -4] = -4*x^2+2*x2+1

(defn print-poly [poly]
  (let [power              (dec (count poly))
        reverse-poly       (reverse poly)
        coefficient        (first reverse-poly)
        second-coefficient (second reverse-poly)
        connector          (if (and second-coefficient (neg? second-coefficient))
                             "-"
                             "+")
        positive-rest      (when second-coefficient (concat (take (dec power) poly) [(Math/abs (* 1.0 second-coefficient))]))
        rest-poly          (if (< 0 power) (print-poly positive-rest) "")]
    (cond
      (<= 2 power) (format "%s*x^%s %s %s" coefficient power connector rest-poly)
      (= 1 power)  (format "%s*x %s %s" coefficient connector rest-poly)
      :else        coefficient)))

(defn normalise-poly
  "Remove 0 at end of poly"
  [p]
  (cond (<= (count p) 1) p
        (not= 0 (last p)) p
        :else (normalise-poly (drop-last p))))

(defn add-poly
  [p1 p2]
  (let [max-length (max (count p1) (count p2))
        p1*        (take max-length (concat p1 (repeat 0)))
        p2*        (take max-length (concat p2 (repeat 0)))]
    (normalise-poly (mapv + p1* p2*))))

(defn sub-poly
  [p1 p2]
  (add-poly p1 (map - p2)))

(defn multiply-poly
  [p1 p2]
  (let [mult-first-c #(* (first p2) %)]
    (if (= (count p2) 1)
      (map mult-first-c p1)
      (add-poly (map mult-first-c p1)
                (multiply-poly (cons 0 p1) (rest p2))))))

(defn eval-poly [p x]
  (if (empty? p)
    0
    (+ (first p) (* x (eval-poly (rest p) x)))))

(defn neville
  "data are distinct [xi,yi],
  i j
  Returns Pij"
  [data i j]
  (let [x #(first (nth data %))]
    (cond
      (= i j) [(second (nth data i))]
      :else (let [multiplier [(double (/ 1.0 (- (x j) (x i))))]
                  top-1 (multiply-poly [(- (x i)) 1]
                                       (neville data (inc i) j))
                  top-2 (multiply-poly [(- (x j)) 1]
                                       (neville data i (dec j)))
                  poly-2 (sub-poly top-1 top-2)
                  res (multiply-poly multiplier poly-2)]
              res))))

(defn neville-fit
  "Returns a function that interpolates a polynomial over these points.
  Points are pairx [xi,yi]"
  [points]
  (let [distinct-points (distinct points)
        n (count distinct-points)]
    (neville distinct-points 0 (dec n))))

(print-poly [1 2 -4])
(print-poly [1 24])

(eval-poly [1 2 -4] 1)

(eval-poly [0 0 1] 3)

(normalise-poly [1 2 -4 0 0])

(add-poly [1 2] [1 2 5 0])

(multiply-poly [1 1] [1 1])

(print-poly (neville-fit [[0 0] [1 1]]))
(print-poly (neville-fit [[0 1] [1 2] [2 5]]))
(print-poly (neville-fit [[0 1] [1 2] [2 5] [4 52]]))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]

  (println (print-poly (neville-fit [[0 0] [1 1]])))
  (println (print-poly (neville-fit [[0 1] [1 2]])))
  (println (print-poly (neville-fit [[0 1] [1 2] [2 5]]))))
