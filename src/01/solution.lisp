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

(defun pt1 (input)
  (loop with lines = (uiop:read-file-lines input)
        with pos = 50
        with count = 0
        for line in lines
        do (match (parse-move line)
             ((cons "L" x)
              (arrows:-<> (- pos x)
                          (mod <> 100)
                          (setq pos <>)))
             ((cons "R" x)
              (arrows:-<> (+ pos x)
                          (mod <> 100)
                          (setq pos <>))))
        if (= pos 0)
          do (incf count)
        finally (return count)))

(defun times-past-0 (dir pos x)
  (loop with p = pos
        with count = 0
        for i from 1 to x
        do (setq p (match dir
                     ("L" (mod (- p 1) 100))
                     ("R" (mod (+ p 1) 100))))
        if (= p 0)
          do (incf count)
        finally (return count)))

(defun pt2 (input)
  (loop with lines = (uiop:read-file-lines input)
        with pos = 50
        with count = 0
        for line in lines
        do (match (parse-move line)
             ((cons "L" x)
              (setq count (+ count (times-past-0 "L" pos x)))
              (arrows:-<> (- pos x)
                          (mod <> 100)
                          (setq pos <>)))
             ((cons "R" x)
              (setq count (+ count (times-past-0 "R" pos x)))
              (arrows:-<> (+ pos x)
                          (mod <> 100)
                          (setq pos <>))))
        finally (return count)))

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
