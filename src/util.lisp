(defpackage #:com.zulu.aoc2019.util
  (:use
   #:alexandria
   #:cl)
  (:export
   #:split))

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
