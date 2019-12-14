(defpackage #:com.zulu.aoc2019.day5
  (:use
   #:alexandria
   #:cl)
  (:local-nicknames
   (#:util #:com.zulu.aoc2019.util)))

(in-package #:com.zulu.aoc2019.day5)

(defvar *int-code*
  #(3 225 1 225
    6 6 1100 1
    238 225 104 0
    1001 210 88 224
    101 -143 224 224
    4 224 1002 223
    8 223 101 3
    224 224 1 223
    224 223 101 42
    92 224 101 -78
    224 224 4 224
    1002 223 8 223
    1001 224 3 224
    1 223 224 223
    1101 73 10 225
    1102 38 21 225
    1102 62 32 225
    1 218 61 224
    1001 224 -132 224
    4 224 102 8
    223 223 1001 224
    5 224 1 224
    223 223 1102 19
    36 225 102 79
    65 224 101 -4898
    224 224 4 224
    102 8 223 223
    101 4 224 224
    1 224 223 223
    1101 66 56 224
    1001 224 -122 224
    4 224 102 8
    223 223 1001 224
    2 224 1 224
    223 223 1002 58
    82 224 101 -820
    224 224 4 224
    1002 223 8 223
    101 3 224 224 1 223 224 223 2 206 214 224 1001 224 -648 224 4 224 102 8 223 223 101 3 224 224 1 223 224 223 1102 76 56 224 1001 224 -4256 224 4 224 102 8 223 223 1001 224 6 224 1 223 224 223 1102 37 8 225 1101 82 55 225 1102 76 81 225 1101 10 94 225 4 223 99 0 0 0 677 0 0 0 0 0 0 0 0 0 0 0 1105 0 99999 1105 227 247 1105 1 99999 1005 227 99999 1005 0 256 1105 1 99999 1106 227 99999 1106 0 265 1105 1 99999 1006 0 99999 1006 227 274 1105 1 99999 1105 1 280 1105 1 99999 1 225 225 225 1101 294 0 0 105 1 0 1105 1 99999 1106 0 300 1105 1 99999 1 225 225 225 1101 314 0 0 106 0 0 1105 1 99999 8 226 677 224 102 2 223 223 1005 224 329 101 1 223 223 1008 677 677 224 1002 223 2 223 1006 224 344 1001 223 1 223 107 226 677 224 102 2 223 223 1005 224 359 1001 223 1 223 1108 677 677 224 1002 223 2 223 1006 224 374 101 1 223 223 1107 677 677 224 1002 223 2 223 1006 224 389 101 1 223 223 108 226 677 224 102 2 223 223 1006 224 404 101 1 223 223 7 677 677 224 102 2 223 223 1006 224 419 101 1 223 223 108 677 677 224 102 2 223 223 1006 224 434 1001 223 1 223 7 226 677 224 102 2 223 223 1006 224 449 1001 223 1 223 108 226 226 224 102 2 223 223 1005 224 464 101 1 223 223 8 226 226 224 1002 223 2 223 1006 224 479 101 1 223 223 1008 226 226 224 102 2 223 223 1005 224 494 1001 223 1 223 1008 677 226 224 1002 223 2 223 1005 224 509 101 1 223 223 7 677 226 224 102 2 223 223 1006 224 524 101 1 223 223 1007 677 226 224 1002 223 2 223 1006 224 539 1001 223 1 223 1108 677 226 224 102 2 223 223 1005 224 554 1001 223 1 223 8 677 226 224 1002 223 2 223 1005 224 569 101 1 223 223 1108 226 677 224 1002 223 2 223 1005 224 584 101 1 223 223 1107 677 226 224 102 2 223 223 1006 224 599 101 1 223 223 107 226 226 224 102 2 223 223 1006 224 614 1001 223 1 223 107 677 677 224 1002 223 2 223 1005 224 629 1001 223 1 223 1107 226 677 224 1002 223 2 223 1006 224 644 101 1 223 223 1007 677 677 224 102 2 223 223 1006 224 659 1001 223 1 223 1007 226 226 224 1002 223 2 223 1006 224 674 1001 223 1 223 4 223 99 226))

(defun decode-instruction (inst)
  "Decode an instruction into its opcode, as well as a list of parameter modes"
  (check-type inst integer)
  (multiple-value-bind (n op) (truncate inst 100)
    (list* op
           (ecase op
             ((1 2)
              (loop
                :repeat 3
                :for x := n :then mod
                :for (mod rem) := (multiple-value-list (truncate x 10))
                :collect rem))
             ((3 4)
              (list (truncate n 10)))
             (99
              nil)))))

(defun run-intcode (intcode)
  (declare (optimize (debug 3)))
  (labels ((ref (pos &optional (offset 0))
             (aref intcode (+ pos offset)))
           ((setf ref) (value pos &optional (offset 0))
             (setf (aref intcode (+ pos offset)) value))
           (mem (pos mode &optional (offset 0))
             (ecase mode
               (0 ; position mode
                (ref (ref (+ pos offset))))
               (1 ; immediate mode
                (ref (+ pos offset)))))
           ((setf mem) (value pos mode &optional (offset 0))
             (ecase mode
               (0 ; position mode
                (setf (ref (ref (+ pos offset))) value))
               (1 ; immediate mode
                (setf (ref (+ pos offset)) value)))))
    (loop
      :for ip := 0 :then (+ ip 1 (list-length param-modes))
      :for (op . param-modes) := (decode-instruction (ref ip))
      :do
         (ecase op
           (1 ;; Sum
            (destructuring-bind (m1 m2 m3) param-modes
              (setf (mem ip m3 3) (+ (mem ip m1 1) (mem ip m2 2)))))
           (2
            (destructuring-bind (m1 m2 m3) param-modes
              (setf (mem ip m3 3) (* (mem ip m1 1) (mem ip m2 2)))))
           (3
            (let ((input (progn
                           (format t "~&Input: ")
                           (read))))
              (check-type input integer)
              (destructuring-bind (m1) param-modes
                (setf (mem ip m1 1) input))))
           (4
            (destructuring-bind (m1) param-modes
              (format t "~&Output: ~A~%" (mem ip m1 1))))
           (99
            (return (values (ref 0) intcode)))))))

(defun part1 (&optional (int-code *int-code*))
  (run-intcode (copy-seq int-code)))

(defvar *int-code-v2* #(3 225 1 225 6 6 1100 1 238 225 104 0 1001 210 88 224 101 -143 224 224 4 224 1002 223 8 223 101 3 224 224 1 223 224 223 101 42 92 224 101 -78 224 224 4 224 1002 223 8 223 1001 224 3 224 1 223 224 223 1101 73 10 225 1102 38 21 225 1102 62 32 225 1 218 61 224 1001 224 -132 224 4 224 102 8 223 223 1001 224 5 224 1 224 223 223 1102 19 36 225 102 79 65 224 101 -4898 224 224 4 224 102 8 223 223 101 4 224 224 1 224 223 223 1101 66 56 224 1001 224 -122 224 4 224 102 8 223 223 1001 224 2 224 1 224 223 223 1002 58 82 224 101 -820 224 224 4 224 1002 223 8 223 101 3 224 224 1 223 224 223 2 206 214 224 1001 224 -648 224 4 224 102 8 223 223 101 3 224 224 1 223 224 223 1102 76 56 224 1001 224 -4256 224 4 224 102 8 223 223 1001 224 6 224 1 223 224 223 1102 37 8 225 1101 82 55 225 1102 76 81 225 1101 10 94 225 4 223 99 0 0 0 677 0 0 0 0 0 0 0 0 0 0 0 1105 0 99999 1105 227 247 1105 1 99999 1005 227 99999 1005 0 256 1105 1 99999 1106 227 99999 1106 0 265 1105 1 99999 1006 0 99999 1006 227 274 1105 1 99999 1105 1 280 1105 1 99999 1 225 225 225 1101 294 0 0 105 1 0 1105 1 99999 1106 0 300 1105 1 99999 1 225 225 225 1101 314 0 0 106 0 0 1105 1 99999 8 226 677 224 102 2 223 223 1005 224 329 101 1 223 223 1008 677 677 224 1002 223 2 223 1006 224 344 1001 223 1 223 107 226 677 224 102 2 223 223 1005 224 359 1001 223 1 223 1108 677 677 224 1002 223 2 223 1006 224 374 101 1 223 223 1107 677 677 224 1002 223 2 223 1006 224 389 101 1 223 223 108 226 677 224 102 2 223 223 1006 224 404 101 1 223 223 7 677 677 224 102 2 223 223 1006 224 419 101 1 223 223 108 677 677 224 102 2 223 223 1006 224 434 1001 223 1 223 7 226 677 224 102 2 223 223 1006 224 449 1001 223 1 223 108 226 226 224 102 2 223 223 1005 224 464 101 1 223 223 8 226 226 224 1002 223 2 223 1006 224 479 101 1 223 223 1008 226 226 224 102 2 223 223 1005 224 494 1001 223 1 223 1008 677 226 224 1002 223 2 223 1005 224 509 101 1 223 223 7 677 226 224 102 2 223 223 1006 224 524 101 1 223 223 1007 677 226 224 1002 223 2 223 1006 224 539 1001 223 1 223 1108 677 226 224 102 2 223 223 1005 224 554 1001 223 1 223 8 677 226 224 1002 223 2 223 1005 224 569 101 1 223 223 1108 226 677 224 1002 223 2 223 1005 224 584 101 1 223 223 1107 677 226 224 102 2 223 223 1006 224 599 101 1 223 223 107 226 226 224 102 2 223 223 1006 224 614 1001 223 1 223 107 677 677 224 1002 223 2 223 1005 224 629 1001 223 1 223 1107 226 677 224 1002 223 2 223 1006 224 644 101 1 223 223 1007 677 677 224 102 2 223 223 1006 224 659 1001 223 1 223 1007 226 226 224 1002 223 2 223 1006 224 674 1001 223 1 223 4 223 99 226))

(defun decode-instruction-v2 (inst)
  "Decode an instruction into its opcode, as well as a list of parameter modes"
  (check-type inst integer)
  (multiple-value-bind (n op) (truncate inst 100)
    (list* op
           (ecase op
             ((1 2 7 8)
              (loop
                :repeat 3
                :for x := n :then mod
                :for (mod rem) := (multiple-value-list (truncate x 10))
                :collect rem))
             ((3 4)
              (list (truncate n 10)))
             ((5 6)
              (loop
                :repeat 2
                :for x := n :then mod
                :for (mod rem) := (multiple-value-list (truncate x 10))
                :collect rem))
             (99
              nil)))))

(defun run-intcode-v2 (intcode)
  (declare (optimize (debug 3)))
  (labels ((ref (pos &optional (offset 0))
             (aref intcode (+ pos offset)))
           ((setf ref) (value pos &optional (offset 0))
             (setf (aref intcode (+ pos offset)) value))
           (mem (pos mode &optional (offset 0))
             (ecase mode
               (0 ; position mode
                (ref (ref (+ pos offset))))
               (1 ; immediate mode
                (ref (+ pos offset)))))
           ((setf mem) (value pos mode &optional (offset 0))
             (ecase mode
               (0 ; position mode
                (setf (ref (ref (+ pos offset))) value))
               (1 ; immediate mode
                (setf (ref (+ pos offset)) value)))))
    (loop
      :for ip := 0 :then (+ ip 1 (list-length param-modes))
      :for (op . param-modes) := (decode-instruction-v2 (ref ip))
      :do
         (ecase op
           (1 ;; Sum
            (destructuring-bind (m1 m2 m3) param-modes
              (setf (mem ip m3 3) (+ (mem ip m1 1) (mem ip m2 2)))))
           (2
            (destructuring-bind (m1 m2 m3) param-modes
              (setf (mem ip m3 3) (* (mem ip m1 1) (mem ip m2 2)))))
           (3
            (let ((input (progn
                           (format t "~&Input: ")
                           (read))))
              (check-type input integer)
              (destructuring-bind (m1) param-modes
                (setf (mem ip m1 1) input))))
           (4
            (destructuring-bind (m1) param-modes
              (format t "~&Output: ~A~%" (mem ip m1 1))))
           (5
            (destructuring-bind (m1 m2) param-modes
              (unless (zerop (mem ip m1 1))
                (setf ip (- (mem ip m2 2) 3)))))
           (6
            (destructuring-bind (m1 m2) param-modes
              (when (zerop (mem ip m1 1))
                (setf ip (- (mem ip m2 2) 3)))))
           (7
            (destructuring-bind (m1 m2 m3) param-modes
              (setf (mem ip m3 3) (if (< (mem ip m1 1) (mem ip m2 2)) 1 0))))
           (8
            (destructuring-bind (m1 m2 m3) param-modes
              (setf (mem ip m3 3) (if (= (mem ip m1 1) (mem ip m2 2)) 1 0))))
           (99
            (return (values (ref 0) intcode)))))))

(defun part2 (&optional (int-code *int-code-v2*))
  (run-intcode-v2 (copy-seq int-code)))
