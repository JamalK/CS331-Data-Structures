(ns linked_lists.core-spec
  (:require [speclj.core :refer :all]
            [linked_lists.core :refer :all])
  (:import [linked_lists.core Cons]))

;; # The Tests
;;
;; We are going to use [spelj](https://github.com/slagyr/speclj) for our tests.


(describe "The record declaration"
          (it "should create something"
              (should (Cons. 10 20)))

          (it "should have a car"
              (should= 10 (:car (Cons. 10 20))))

          (it "should have a cdr"
              (should= 20 (:cdr (Cons. 10 20))))

          (it "should be chainable"
              (should= 40 (-> (Cons. 10 (Cons. 20 (Cons. 30 40))) :cdr :cdr :cdr))))

(describe "insert-at-beginning"
          (it "creates a cons cell"
              (should-not= nil (insert-at-beginning 10 nil)))

          (it "should work with empty lists"
              (should= (Cons. 10 nil) (insert-at-beginning 10 nil) ))

          (it "should work with lists that have data"
              (let [xx (Cons. 10 (Cons. 20 (Cons. 30 nil)))]
                (should= (Cons. 5 xx) (insert-at-beginning 5 xx) )))

          (it "should insert at the beginning"
            (let [xx (Cons. 10 (Cons. 20 (Cons. 30 nil)))]
                (should= 5 (-> (insert-at-beginning 5 xx) :car ))))
          )



(describe "insert-at-end"
          (it "Creates a cons cell."
              (should-not= nil (insert-at-end 10 nil)))


          (it "should work with empty lists"
              (should= (Cons. 10 nil) (insert-at-end 10 nil) ))


          (it "should work with lists that have data"
              (let [xx (Cons. 10 (Cons. 20 (Cons. 30 nil)))]
                (should= (Cons. (:car xx) (insert-at-end  5 (:cdr xx))) (insert-at-end 5 xx))))

          (it "should add elements to the end"
              (let [xx (Cons. 10 (Cons. 20 (Cons. 30 nil)))]
              (should= 40 (-> (insert-at-end 40 xx) :cdr :cdr :cdr :car) )))

          (it "should be chainable"
              (let [xx (Cons. 10 (Cons. 20 nil))]
              (should= 30 (-> (insert-at-end 30 xx) :cdr :cdr :car))))
)

(describe "sorted insert"
          (it "should create a Cons cell"
              (should-not= nil (sorted-insert 10 nil)))

          (it "Should work with empty lists."
              (should= (Cons. 10 nil) (sorted-insert 10 nil)))

          (it "should Add an element to the list"
              (let [xx (Cons. 1 (Cons. 3 nil))]
              (should= 2 (-> (sorted-insert 2 xx) :cdr :car))))

          (it "should have bigger value than element before it"
              (let [xx (Cons. 1 (Cons. 2 (Cons. 4 nil)))]
                (should= true (< (-> (sorted-insert 3 xx) :cdr :cdr :car) (-> (sorted-insert 3 xx) :cdr :cdr :cdr :car)))))

)

(describe "search"

          (it "Should Return the number if found."
              (let [xx (Cons. 10 (Cons. 20 (Cons. 30 nil)))]
              (should (search 20 xx))))

          (it "Should retrun nil if not found."
          (let [xx (Cons. 10 (Cons. 20 (Cons. 30 nil)))]
              (should-not (search 40 xx))))




)

(describe "delete"
          (it "Should return the list if element wasn't found."
              (let [xx (Cons. 10 (Cons. 20 (Cons. 30 nil)))]
               (should= xx (delete 40 xx))))

          (it "should remove element from list if found and return a copy."
               (let [xx (Cons. 10 (Cons. 20 (Cons. 30 nil)))]
               (should= (Cons. 10 (Cons. 30 nil)) (delete 20 xx))))

          (it "should delete the element once."
             (let [xx (Cons. 10 (Cons. 20 (Cons. 20 nil)))]
               (should= (Cons. 10 (Cons. 20 nil)) (delete 20 xx) )))

          (it "should be chainable"
              (let [xx (Cons. 10 (Cons. 20 nil))]
              (should= 30 (-> (insert-at-end 30 xx) :cdr :cdr :car))))

          (it "should create a Cons cell"
              (should-not= (Cons. 10 nil) (delete 10 nil)))
)

(describe "delete-all"
         (it "Should return the list if element wasn't found."
              (let [xx (Cons. 10 (Cons. 20 (Cons. 30 nil)))]
               (should= xx (delete-all 40 xx))))

          (it "should remove element from list if found and return a copy."
               (let [xx (Cons. 10 (Cons. 20 (Cons. 30 nil)))]
               (should= (Cons. 10 (Cons. 30 nil)) (delete-all 20 xx))))

          (it "shouldn't delete just one copy of the list."
              (let [xx (Cons. 10 (Cons. 20 (Cons. 20 nil)))]
                (should-not= (Cons. 10 (Cons. 20 nil)) (delete-all 20 xx))))

          (it "should remove ALL elements from list if found and return a copy."
               (let [xx (Cons. 10 (Cons. 20 (Cons. 20 nil)))]
               (should= (Cons. 10 nil) (delete-all 20 xx))))

          (it "should create a Cons cell"
              (should-not= (Cons. 10 nil) (delete-all 10 nil)))
)

(describe "efficient-delete"
          (it "should return the same list if it can't find the element"
              (let [xx (Cons. 10 (Cons. 20. (Cons. 30 nil)))]
                (should (identical? xx (efficient-delete 45 xx)))))

          (it "should delete the element once."
             (let [xx (Cons. 10 (Cons. 20 (Cons. 20 nil)))]
               (should= (Cons. 10 (Cons. 20 nil)) (efficient-delete 20 xx) )))

          (it "should remove element from list if found and return a copy."
               (let [xx (Cons. 10 (Cons. 20 (Cons. 30 nil)))]
               (should= (Cons. 10 (Cons. 30 nil)) (efficient-delete 20 xx))))
)
(run-specs)
