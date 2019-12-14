(defsystem #:com.zulu.aoc2019
  :version "0.0.0"
  :description "Advent of Code 2019 Solutions"
  :author "Wilfredo Velázquez-Rodríguez <zulu.inuoe@gmail.com>"
  :license "CC0 1.0 Universal"
  :serial t
  :components
  ((:file "util")
   (:file "day1")
   (:file "day2")
   (:file "day3")
   (:file "day4"))
  :depends-on
  (#:alexandria))
