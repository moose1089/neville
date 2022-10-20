(ns neville.core
  (:gen-class))

;; Polynomials represented by coefficients in reverse order
;; [1 2 -4] = -4*x^2+2*x2+1


(defn neville
  "Returns a function that interpolates a polynomial over these points.
  Points are pairx [xi,yi]"
  [points]
  (let [distinct-points (distinct points)
        data {}]
    (map (juxt identity ))
   )
 )

(defn print-poly [poly]
  (let [power              (dec (count poly))
        reverse-poly       (reverse poly)
        coefficient        (first reverse-poly)
        second-coefficient (second reverse-poly)
        connector          (if (and second-coefficient (neg? second-coefficient))
                             "-"
                             "+")
        positive-rest      (when second-coefficient (cons (Math/abs second-coefficient) (take (dec power) poly)))
        rest-poly          (if (< 0 power) (print-poly positive-rest) "")]
    (cond
      (<= 2 power) (format "%d*x^%d %s %s" coefficient power connector rest-poly)
      (= 1 power)  (format "%d*x %s %s" coefficient connector rest-poly)
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

(defn multiply-poly
  [p1 p2])

(print-poly [1 2 -4])

(normalise-poly [1 2 -4 0 0])

(add-poly [1 2] [1 2 5 0])

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
