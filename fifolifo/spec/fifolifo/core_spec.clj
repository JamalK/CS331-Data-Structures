(ns fifolifo.core-spec
  (:refer-clojure :exclude [pop peek])
  (:require [speclj.core :refer :all]
            [fifolifo.core :refer :all])
  (:import [fifolifo.core Stack Queue]))

;; # The Tests
;;
;; We are going to use [spelj](https://github.com/slagyr/speclj) for our tests.


(describe "The stack declaration"

          (it "should create something."
              (should (make-stack)))

          (it "should have empty components."
              (should= (Stack. nil 0) (make-stack)))

          (it "should have a size of zero."
              (should= 0 (stack-size (make-stack))))
          )


(describe "The Stack size"

          (it "should return positive value."
              (let [stk (Stack. '(5 3 2 1) 4)]
                (should= (:size stk) (stack-size stk))))

          (it "should return value bigger than 0"
              (let [stk (Stack. '(5 3 2 1) 4)]
                (should (<= 0 (stack-size stk)))))

          (it "should have proper size with number of elements"
              (let [stk (Stack. '(5 3 2 1) 4)]
              (should (= (:size stk) (count (:top stk) ) ) )))
          )


(describe "Push Stack"
          (it "increment the size."
              (let [stk (make-stack)]
                (should= 1 (:size (push stk 20)))))

          (it "Should add an element at the beginning"
              (let [stk (Stack. '(5 3 2 1) 4)]
                (should= 7 (top (push stk 7)))))


          (it "should return the same list"
              (let [stk (Stack. '(5 3 2 1) 4)]
                (should= '(7 5 3 2 1) (:top (push stk 7)))))

          (it "should work"
              (let [stk (make-stack)]
                (should= (Stack. '(1) 1) (push stk 1))))

          )

(describe "Pop"
          (it "decrement the size."
              (let [stk (Stack. '(8 5 3 2 1) 5)]
                (should= 4 (stack-size (pop stk)))))

          (it "shouldn't decrement when it's nil"
              (let [stk (make-stack)]
                (should= 0 (stack-size (pop stk)))))

          (it "Should remove element from the beginning"
              (let [stk (Stack. '(5 3 2 1) 4)]
                (should= 3 (top (pop stk)))))

          (it "Should return same list when poping"
            (let [stk (Stack. '(5 3 2 1) 4)]
                (should= (Stack. '(3 2 1) 3) (pop stk))))



          (it "Should return the rest of the elements in correct order"
             (let [stk (Stack. '(5 3 2 1) 4)]
                (should= '(3 2 1) (:top (pop stk))))
              )


          )


(describe "The queue declaration"

          (it "should create something."
              (should (make-queue)))

          (it "should have empty components."
              (should= (Queue. nil nil 0) (make-queue)))

          (it "should have a size of zero."
              (should= 0 (stack-size (make-stack))))
          )


(describe "The Enqueue function"

          (it "should increment size"
              (let [q (make-queue)]
                (should= 1 (queue-size (enqueue q 10)))))


          )

(describe "The Dequeue function"

          (it "should decrement the size"
              (let [q (Queue. '(1 2 3 4) nil 4)]
                (should= 3 (queue-size (dequeue q)))))

          (it "shouldn't give negative sizes"
              (let [q (make-queue)]
                (should= 0 (queue-size(dequeue q)))))

          (it "should reverse when moving"
               (let [q (Queue. '(1 2 3 4) nil 4)]
                (should= '(3 2 1) (:front (dequeue q)))))




          )



;; (describe "inaction"

;;           (it "should have some tests at some point."
;;               (should true)))

(run-specs)
