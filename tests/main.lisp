(defpackage filepaths/tests/main
  (:use :cl
        :filepaths
        :rove))
(in-package :filepaths/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :filepaths)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
