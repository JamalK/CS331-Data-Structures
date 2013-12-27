

(ns first_one.core)

;; here is the foo function 
(defn foo [x y]
  "A function to test if x > y."
  (> x y))
;; take the absolute value. I think it's buggy.
(defn abs [x]
  (if (> x 0) (- x) x))

(defn bar [x y ]
  "A function to add x to y."
   (+ (abs x) (abs y)))
