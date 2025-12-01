;; advent of code 2025 - day 01
;; https://adventofcode.com/2025/day/1

(in-package :ciel-5am-user)

(defun parse-move (str)
  (-> (str:split "" str :omit-nulls t)
      (lambda (lst)
        (cons (car lst)
              (-> (cdr lst)
                  (serapeum:string-join "")
                  (parse-number))))))

(defun next-pos (pos dir val)
  (-> (match dir
        ("L" (- pos val))
        ("R" (+ pos val)))
      (mod 100)))

(defun pt1 (input)
  (loop with lines = (uiop:read-file-lines input)
        with pos = 50
        with times = 0
        for line in lines
        do (bind (((dir . val) (parse-move line)))
             (setq pos (next-pos pos dir val)))
        if (= pos 0)
          do (incf times)
        finally (return times)))

(defun next-pos-v2 (pos dir val)
  (loop with p = pos
        with times = 0
        for i from 1 to val
        do (setq p (match dir
                     ("L" (mod (- p 1) 100))
                     ("R" (mod (+ p 1) 100))))
        if (= p 0)
          do (incf times)
        finally (return (cons times p))))

(defun pt2 (input)
  (loop with lines = (uiop:read-file-lines input)
        with pos = 50
        with times = 0
        for line in lines
        do (bind (((dir . val) (parse-move line))
                  ((tms . nxt) (next-pos-v2 pos dir val)))
             (setq times (+ times tms))
             (setq pos nxt))
        finally (return times)))

(def-suite 01-solution)
(in-suite 01-solution)

(test pt1-test
  (is (= (pt1 "test.txt") 3)))

(test pt1-input
  (is (= (pt1 "input.txt") 1081)))

(test pt2-test
  (is (= (pt2 "test.txt") 6)))

(test pt2-input
  (is (= (pt2 "input.txt") 6689)))

(defun main ()
  (run! '01-solution))

#+ciel
(main)
