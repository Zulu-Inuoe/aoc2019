(defpackage #:com.zulu.aoc2019.day4
  (:use
   #:alexandria
   #:cl)
  (:local-nicknames
   (#:util #:com.zulu.aoc2019.util)))

(in-package #:com.zulu.aoc2019.day4)

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
  (let ((digits (util:split-digits n)))
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

(defun group-adjacent (seq &key test)
  (setf test (or test #'eql))
  (let ((groups ())
        (last-group nil))
    (map nil (lambda (elt)
               (cond
                 ((or (null last-group)
                      (funcall test elt (car last-group)))
                  (push elt last-group))
                 (t
                  (push (nreverse last-group) groups)
                  (setf last-group (list elt)))))
         seq)
    (when last-group
      (push (nreverse last-group) groups))
    (nreverse groups)))

(defun adjacent-digits-same-v2-p (digits)
   "Returns true if there is one pair of exactly two adjacent digits are equal in `digits'

 (adjacent-digits-same-v2-p '(1 1 1) ; => NIL
 (adjacent-digits-same-v2-p '(1 2 3) ; => NIL
 (adjacent-digits-same-v2-p '(1 1 1 2 2)) ; => T
"
  (loop
    :for group :in (group-adjacent digits)
      :thereis (= (list-length group) 2)))

(defun valid-number-v2-p (n)
  "Returns true if `n' is a valid password number as per part2 rules."
  (let ((digits (util:split-digits n)))
    (and (= (list-length digits) 6)
         (adjacent-digits-same-v2-p digits)
         (non-decreasing-p digits))))

(defun part2 (&optional (min 235741) (max 706948))
  (when (or (< 999999 min) (< max 99999))
    (return-from part2 0))
  (unless (<= min max)
    (error "min must be below max"))
  (loop
    :for n :from min :to max
    :count (valid-number-v2-p n)))
