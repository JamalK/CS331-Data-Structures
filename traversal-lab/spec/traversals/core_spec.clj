(ns traversals.core-spec
  (:require [speclj.core :refer :all]
            [traversals.core :refer :all])
  (:import [traversals.core BNode])
  )

;; # The Tests
;;
;; We are going to use [spelj](https://github.com/slagyr/speclj) for our tests.

(def k (reduce add nil '(4 2 6 1 3 5 7)))

(describe "Preorder"
          (it "Should return elements in the correct order."
              (should= '(4 2 1 3 6 5 7) (preorder k))))


(describe "Postorder"
          (it "should return elements in correct order."
              (should= '(1 3 2 5 7 6 4) (postorder k))))

(describe "Inorder"
          (it "Should return elements in correct order"
              (should= '(1 2 3 4 5 6 7) (inorder k))))
;;

(run-specs)
