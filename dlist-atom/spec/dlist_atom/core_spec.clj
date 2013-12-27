(ns dlist-atom.core-spec
;  (:refer-clojure :exclude [])
  (:require [speclj.core :refer :all]
            [dlist-atom.core :refer :all])
;  (:import [dlist-atom.core ])
  )

;; # The Tests
;;
;; We are going to use [spelj](https://github.com/slagyr/speclj) for our tests.


(describe "insert-front"
  (it "should work for an empty dlist"
    (let [xx (dlist)]
      (insert-front xx 2)
      (should= '(2) (show-dlist xx))))

  (it "should set link to previous"
    (let [xx (dlist)]
      (insert-front xx 1)
      (insert-front xx 3)
      (should (identical? (-> xx d-sentinel d-prev d-next) (-> xx d-sentinel d-next d-prev)))))



  (it "should increment the number of elements "
          (let [xx (dlist)]
            (insert-front xx 2)
            (should= 1 (d-size xx))))





  (it "should return proper list "
          (let [xx (dlist)]
            (insert-front xx '1)
            (insert-front xx '2)
            (insert-front xx '3)
            (should= '(3 2 1) (show-dlist xx)))
      )
          )


(describe "insert-last"
   (it "should work for an empty dlist"
    (let [xx (dlist)]
      (insert-last xx 2)
      (should= '(2) (show-dlist xx))))


   (it "should work for a list that already has an element"
    (let [xx (dlist)]
      (insert-last xx 2)
      (insert-last xx 4)
      (should= '(2 4) (show-dlist xx))))

 (it "should return proper list "
          (let [xx (dlist)]
            (insert-last xx '1)
            (insert-last xx '2)
            (should= '(1 2) (show-dlist xx)))
      )




  (it "should increment the number of elements "
       (let [xx (dlist)]
         (insert-last xx 2)
          (should= 1 (d-size xx))))
          )


(describe "insert-sorted"


      (it "shouldn't miss a back link"
          (let [xx (dlist)]

            (insert-sorted xx 20)
            (should= 20 (-> xx d-sentinel d-prev d-data))
         ))
)

(describe "Index forward"
          (it "shouldn't return nil"
              (let [xx (dlist)]
               (insert-front xx 30)
                (insert-front xx 20)
                (insert-front xx 40)
                (insert-front xx 50)

               (should= nil (index-forward xx 90)))
          )
)

(describe "Index backward"
          (it "shouldn return positive numbers"
              (let [xx (dlist)]
               (insert-front xx 30)
                (insert-front xx 20)
                (insert-front xx 40)
                (insert-front xx 50)

               (should (>= 0 (index-backward xx 20)))
          ))


          (it "Should index properly"
             (let [xx (dlist)]
               (insert-front xx 30)
                (insert-front xx 20)
                (insert-front xx 40)
                (insert-front xx 50)

               (should= -2 (index-backward xx 20))) )


          )



(describe "delete"
          (it "Assumes list is sorted"
               (let [xx (dlist)]
               (do (insert-front xx 12)
               (insert-front xx 34)
               (insert-front xx 16)
               (insert-front xx 10)
               (delete xx 16)
               (should= '(10 34 12) (show-dlist xx)))))



          )



(describe "List to Dlist"
;;           (it "shouldn't missup with pointers"
;;              (let [f '(1 2)]
;;             (should (identical? (-> (list-to-dlist f) d-sentinel d-prev d-next) (-> (list-to-dlist f) d-sentinel d-next d-prev)))))



          (it "should return preoperly"
              (should= '(1 2 3 4) (show-dlist (list-to-dlist '(1 2 3 4))))
          )
          )






;;           (it "should turn list to dlist properly"
;;               (should= '(1 2) (show-dlist (list-to-dlist '(1 2)))))

;;                       )

    (describe "show dlist revrse"
              (it "should display something"
              (let [xx (dlist)]
               (insert-front xx 12)
               (insert-front xx 34)
               (insert-front xx 16)
               (insert-front xx 10)
                 (should= '(12 34 16 10) (show-dlist-reverse xx)))))






(run-specs)
