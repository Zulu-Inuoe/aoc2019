(defpackage #:com.zulu.aoc2019.day4
  (:use
   #:alexandria
   #:cl)
  (:local-nicknames
   (#:util #:com.zulu.aoc2019.util)))

(in-package #:com.zulu.aoc2019.day4)

(defun split-digits (n)
  "Create a list of digits from the integer `n'"
  (check-type n integer)
  (let ((digits ()))
    (loop
      :for x := n :then mod
      :until (zerop x)
      :for (mod rem) := (multiple-value-list (truncate x 10))
      :do (push rem digits))
    digits))

(defun adjacent-digits-same-p (digits)
  "Returns true if two adjacent digits are equal in `digits'

 (adjacent-digits-same-p '(1 1 1) ; => T
 (adjacent-digits-same-p '(1 2 3) ; => NIL
"
  (loop
    :for (a b) :on digits
    :while b
      :thereis (= a b)))

(defun non-decreasing-p (digits)
  "Returns true if all `digits' are non-decreasing.

 (non-decreasing-p '(1 1 1)) ; => T
 (non-decreasing-p '(1 2 1)) ; => NIL
"
  (loop
    :for (a b) :on digits
    :while b
    :always (<= a b)))

(defun valid-number-p (n)
  "Returns true if `n' is a valid password number as per part1 rules."
  (let ((digits (split-digits n)))
    (and (= (list-length digits) 6)
         (adjacent-digits-same-p digits)
         (non-decreasing-p digits))))

(defun part1 (&optional (min 235741) (max 706948))
  (when (or (< 999999 min) (< max 99999))
    (return-from part1 0))
  (unless (<= min max)
    (error "min must be below max"))
  (loop
    :for n :from min :to max
    :count (valid-number-p n)))
