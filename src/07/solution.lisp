;; advent of code 2025 - day 07
;; https://adventofcode.com/2025/day/7

(in-package :ciel-5am-user)

(defun pt1 (input)
  (loop with lines = (->> (uiop:read-file-lines input)
                       (mapcar (lambda (line)
                                 (str:split-omit-nulls " " line))))
        do (print lines)
  ))

(def-suite 07-solution)
(in-suite 07-solution)

(test pt1-test
  (is (= (pt1 "test.txt") 4277556)))

(defun main ()
  (run! '07-solution))

#+ciel
(main)
