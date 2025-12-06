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

(defun merge-ranges (r1 r2)
  (bind (((lo1 hi1) r1)
         ((lo2 hi2) r2))
    (when (and (>= lo2 lo1)
               (<= lo2 hi1))
      (list lo1 (max hi1 hi2)))))

(defun sum-ranges (ranges)
  (loop with total = 0
        for (lo hi) in ranges
        do (setq total (+ total (1+ (- hi lo))))
        finally (return total)))

(defun pt2 (input)
  (loop with data = (-> (uiop:read-file-lines input)
                        (parse-inv))
        with ranges = (-> (nth 0 data)
                          (sort (lambda (a b)
                                  (< (car a) (car b)))))
        with cur = nil
        with merged = nil
        for range in ranges
        if (eq cur nil)
          do (setq cur range)
        else
          do (-> (merge-ranges cur range)
                 (lambda (res)
                   (if (eq res nil)
                       (progn
                         (push cur merged)
                         (setq cur range))
                       (setq cur res))))
        finally (-> (push cur merged)
                    (sum-ranges)
                    (return))))

(def-suite 05-solution)
(in-suite 05-solution)

(test pt1-test
  (is (= (pt1 "test.txt") 3)))

(test pt1-input
  (is (= (pt1 "input.txt") 611)))

(test pt2-test
  (is (= (pt2 "test.txt") 14)))

(test pt2-input
  (is (= (pt2 "input.txt") 345995423801866)))

(defun main ()
  (run! '05-solution))

#+ciel
(main)
