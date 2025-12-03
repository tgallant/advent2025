;; advent of code 2025 - day 02
;; https://adventofcode.com/2025/day/2

(in-package :ciel-5am-user)

(defun parse-range (s)
  (-> (str:split "-" s)
      (lambda (x)
        (bind (((lo hi) x))
          (cons (parse-number lo)
                (parse-number hi))))))

(defun dubp (i)
  (-> (write-to-string i)
      (lambda (s)
        (-> (length s)
            (lambda (l)
              (cond ((oddp l) nil)
                    (t (equal (str:substring 0 (/ l 2) s)
                              (str:substring (/ l 2) l s)))))))))

(defun pt1 (input)
  (loop with ranges = (->> (uiop:read-file-string input)
                           (str:split ",")
                           (mapcar #'parse-range))
        with sum = 0
        for (lo . hi) in ranges
        do (loop for i from lo to hi
                 if (dubp i)
                   do (setq sum (+ sum i)))
        finally (return sum)))

(defun split-into-chunks (seq size)
  (loop for i from 0 by size to (length seq)
        as chunk = (subseq seq i (min (+ i size) (length seq)))
        while chunk
        collect chunk))

(defun all-sublists-equal-p (list-of-lists)
  (if (null list-of-lists) t
      (let ((first-sublist (car list-of-lists)))
        (every #'(lambda (sublist) (equal first-sublist sublist))
               (cdr list-of-lists)))))

(defun dubp-v2 (i)
  (loop with seq = (str:split "" (write-to-string i) :omit-nulls t)
        with len = (length seq)
        for n from 1 to (/ len 2)
        if (-> (split-into-chunks seq n)
               (all-sublists-equal-p))
          do (return t)))

(defun pt2 (input)
  (loop with ranges = (->> (uiop:read-file-string input)
                           (str:split ",")
                           (mapcar #'parse-range))
        with sum = 0
        for (lo . hi) in ranges
        do (loop for i from lo to hi
                 if (dubp-v2 i)
                   do (setq sum (+ sum i)))
        finally (return sum)))

(def-suite 02-solution)
(in-suite 02-solution)

(test pt1-test
  (is (= (pt1 "test.txt") 1227775554)))

(test pt1-input
  (is (= (pt1 "input.txt") 40055209690)))

(test pt2-test
  (is (= (pt2 "test.txt") 4174379265)))

(test pt2-input
  (is (= (pt2 "input.txt") 50857215650)))

(defun main ()
  (run! '02-solution))

#+ciel
(main)
