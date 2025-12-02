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

(def-suite 02-solution)
(in-suite 02-solution)

(test pt1-test
  (is (= (pt1 "test.txt") 1227775554)))

(test pt1-input
  (is (= (pt1 "input.txt") 40055209690)))

(defun main ()
  (run! '02-solution))

#+ciel
(main)
