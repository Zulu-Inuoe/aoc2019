(defpackage #:com.zulu.aoc2019.day1
  (:use
   #:alexandria
   #:cl)
  (:local-nicknames
   (#:util #:com.zulu.aoc2019.util)))

(in-package #:com.zulu.aoc2019.day1)

(defvar *input-modules* '(83281 110963 137849 105456 112819 60817 72085 61440 71799 87704 106917 60141 98846 101962 119935 105419 148806 59017 106495 63871 70045 74235 148702 60455 77694 140310 86284 84659 123898 69894 139427 94767 79377 66250 84478 135686 67196 52581 110081 54347 84698 130634 127325 92776 126100 56838 86543 113360 72062 111919 74682 103605 147243 141504 59943 72751 98896 81071 89513 83074 113120 70692 76552 111705 137550 61939 74620 60464 104956 121073 91999 81857 68973 115985 50815 68344 146640 117467 122904 122521 70758 53028 147377 140588 54506 80064 145885 66725 60104 127545 137801 117472 99427 126069 126418 102451 116782 66106 81694 139492))

(defun calc-fuel (mass)
  "Basic fuel cost equation for a module with `mass'"
  (-  (truncate mass 3) 2))

(defun part1 (&optional (input-modules *input-modules*))
  (reduce #'+ input-modules :key #'calc-fuel))

(defun calc-fuel+ (mass)
  "Calculates fuel costs of a module with `mass' while also calculating cost of additional fuel."
  (labels ((recurse (mass sum)
             (let ((fuel (calc-fuel mass)))
               (if (<= fuel 0)
                   sum
                   (recurse fuel (+ sum fuel))))))
    (recurse mass 0)))

(defun part2 (&optional (input-modules *input-modules*))
  (reduce #'+ input-modules :key #'calc-fuel+))
