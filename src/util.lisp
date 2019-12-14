(defpackage #:com.zulu.aoc2019.util
  (:use
   #:alexandria
   #:cl)
  (:export
   #:make-displaced
   #:split
   #:split-digits))

(in-package #:com.zulu.aoc2019.util)

(defun make-displaced (vector start &optional end)
  (make-array (- (or end (length vector)) start)
              :element-type (array-element-type vector)
              :displaced-to vector
              :displaced-index-offset start))

(defun split (vector &optional (separator #\,))
  "Splits up a vector by `separator'"
  (loop
    :with len := (length vector)
    :for start := 0 :then (1+ end)
    :while (< start len)
    :for end := (or (position separator vector :start start) len)
    :collect (make-displaced vector start end)))

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
