(defpackage keval/tests/main
  (:use :cl
        :keval
        :rove))
(in-package :keval/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :keval)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
