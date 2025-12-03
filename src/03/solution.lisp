;; advent of code 2025 - day 03
;; https://adventofcode.com/2025/day/3

(in-package :ciel-5am-user)

(defun pt1 (input)
  (loop with lines = (->> (uiop:read-file-lines input)
                          (mapcar (lambda (line)
                                    (str:split-omit-nulls "" line))))
        with hi = 0
        with sum = 0
        for line in lines
        do (loop for idx1 from 0 to (1- (length line))
                 do (loop for idx2 from (1+ idx1) to (1- (length line))
                          do (-> (concatenate 'string (nth idx1 line) (nth idx2 line))
                                 (parse-number)
                                 (lambda (n)
                                   (when (> n hi)
                                     (setq hi n))))))
        do (setq sum (+ sum hi))
        do (setq hi 0)
        finally (return sum)))

(defun find-next-pos (lst n)
  (loop with next-pos = 0
        with hi = 0
        for i from 0 to (- (length lst) n)
        do (-> (nth i lst)
               (parse-number)
               (lambda (n)
                 (when (> n hi)
                   (setq next-pos i)
                   (setq hi n))))
        finally (return next-pos)))

(defun maxn (l n)
  (loop with cur = l
        with idx = n
        with result = ""
        repeat n
        do (-> (find-next-pos cur idx)
               (lambda (pos)
                 (setq result (concatenate 'string result (nth pos cur)))
                 (setq cur (subseq cur (1+ pos)))))
        do (setq idx (1- idx))
        finally (-> (parse-number result)
                    (return))))

(defun pt2 (input)
  (loop with lines = (->> (uiop:read-file-lines input)
                          (mapcar (lambda (line)
                                    (str:split-omit-nulls "" line))))
        with sum = 0
        for line in lines
        do (->> (maxn line 12)
                (+ sum)
                (setq sum))
        finally (return sum)))

(def-suite 03-solution)
(in-suite 03-solution)

(test pt1-test
  (is (= (pt1 "test.txt") 357)))

(test pt1-input
  (is (= (pt1 "input.txt") 17144)))

(test pt2-test
  (is (= (pt2 "test.txt") 3121910778619)))

(test pt2-input
  (is (= (pt2 "input.txt") 170371185255900)))

(defun main ()
  (run! '03-solution))

#+ciel
(main)
