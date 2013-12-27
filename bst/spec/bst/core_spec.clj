(ns bst.core-spec
;  (:refer-clojure :exclude [])
  (:require [speclj.core :refer :all]
            [bst.core :refer :all])
;  (:import [bst.core ])
  )

;; # The Tests
;;
;; We are going to use [spelj](https://github.com/slagyr/speclj) for our tests.


(describe "The spec file"
          (it "should have some tests."
              (should false)))

(run-specs)
