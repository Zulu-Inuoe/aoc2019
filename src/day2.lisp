(defpackage #:com.zulu.aoc2019.day2
  (:use
   #:alexandria
   #:cl)
  (:local-nicknames
   (#:util #:com.zulu.aoc2019.util)))

(in-package #:com.zulu.aoc2019.day2)

(defun run-intcode (intcode)
  (labels ((ref (pos &optional (offset 0))
             (aref intcode (+ pos offset)))
           ((setf ref) (value pos &optional (offset 0))
             (setf (aref intcode (+ pos offset)) value))
           (lea (pos &optional (offset 0))
             (ref (ref pos offset)))
           ((setf lea) (value pos &optional (offset 0))
             (setf (ref (ref pos offset)) value)))
    (loop
      :for ip :from 0 :by 4
      :for op := (ref ip)
      :do
         (ecase op
           (1 ;; Sum
            (setf (lea ip 3) (+ (lea ip 1) (lea ip 2))))
           (2
            (setf (lea ip 3) (* (lea ip 1) (lea ip 2))))
           (99
            (return (values (ref 0) intcode)))))))

(defvar *intcode* #(1 0 0 3
                    1 1 2 3
                    1 3 4 3
                    1 5 0 3
                    2 6 1 19
                    1 19 5 23
                    2 10 23 27
                    2 27 13 31
                    1 10 31 35
                    1 35 9 39
                    2 39 13 43
                    1 43 5 47
                    1 47 6 51
                    2 6 51 55
                    1 5 55 59
                    2 9 59 63
                    2 6 63 67
                    1 13 67 71
                    1 9 71 75
                    2 13 75 79
                    1 79 10 83
                    2 83 9 87
                    1 5 87 91
                    2 91 6 95
                    2 13 95 99
                    1 99 5 103
                    1 103 2 107
                    1 107 10 0
                    99 2 0 14
                    0))

(defun part1 (&optional (intcode *intcode*))
  "Run the intcode program with a couple of spots muddled up."
  (let ((tmp (copy-seq intcode)))
    ;; Restore to before the fire
    (setf (aref tmp 1) 12)
    (setf (aref tmp 2) 2)
    (run-intcode tmp)))


(defun part2 (&optional (intcode *intcode*))
  "Brute-force search for combinations of values that produce the code 19690720"
  (dotimes (noun 99)
    (dotimes (verb 99)
      (let ((tmp (copy-seq intcode)))
        (setf (aref tmp 1) noun
              (aref tmp 2) verb)
        (when (= 19690720 (run-intcode tmp))
          (return-from part2 (+ (* noun 100) verb)))))))
