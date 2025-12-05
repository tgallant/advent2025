;; advent of code 2025 - day 05
;; https://adventofcode.com/2025/day/5

(in-package :ciel-5am-user)

(defun parse-range (line)
  (->> (str:split "-" line)
       (mapcar (lambda (val)
                 (parse-number val)))))

(defun parse-inv (lines)
  (loop with ranges = nil
        with ids = nil
        with sep = nil
        for line in lines
        if (equal line "")
          do (setq sep t)
        else
          do (if (eq sep t)
                 (-> (parse-number line)
                     (push ids))
                 (-> (parse-range line)
                     (push ranges)))
        finally (return (list ranges ids))))

(defun in-range-p (num ranges)
  (loop for (lo hi) in ranges
        if (and (>= num lo)
                (<= num hi))
          do (return t)))

(defun pt1 (input)
  (loop with data = (-> (uiop:read-file-lines input)
                        (parse-inv))
        with ranges = (nth 0 data)
        with ids = (nth 1 data)
        with total = 0
        for id in ids
        if (in-range-p id ranges)
          do (incf total)
        finally (return total)))

(def-suite 05-solution)
(in-suite 05-solution)

(test pt1-test
  (is (= (pt1 "test.txt") 3)))

(test pt1-input
  (is (= (pt1 "input.txt") 611)))

(defun main ()
  (run! '05-solution))

#+ciel
(main)
