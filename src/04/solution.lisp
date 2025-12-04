;; advent of code 2025 - day 04
;; https://adventofcode.com/2025/day/4

(in-package :ciel-5am-user)

(defun getval (grid r c)
  (cond ((< r 0) "")
        ((< c 0) "")
        ((>= r (length grid)) "")
        ((>= c (length (car grid))) "")
        (t (nth c (nth r grid)))))

(defun make-adj (r c)
  (list (list r (1+ c))
        (list r (1- c))
        (list (1+ r) c)
        (list (1- r) c)
        (list (1+ r) (1+ c))
        (list (1+ r) (1- c))
        (list (1- r) (1+ c))
        (list (1- r) (1- c))))

(defun count-adj (grid r c)
  (loop with adj = (make-adj r c)
        with num = 0
        for (ar ac) in adj
        do (-> (getval grid ar ac)
               (equal "@")
               (when (incf num)))
        finally (return num)))

(defun pt1 (input)
  (loop with lines = (->> (uiop:read-file-lines input)
                          (mapcar (lambda (line)
                                    (str:split-omit-nulls "" line))))
        with num = 0
        with rlen = (length (car lines))
        with clen = (length lines)
        for r from 0 to (1- rlen)
        do (loop for c from 0 to (1- clen)
                 do (-> (getval lines r c)
                        (lambda (val)
                          (when (and (equal val "@")
                                     (-> (count-adj lines r c)
                                         (< 4)))
                            (incf num)))))
        finally (return num)))

(defun pt2 (input)
  (loop with lines = (->> (uiop:read-file-lines input)
                          (mapcar (lambda (line)
                                    (str:split-omit-nulls "" line))))
        with total = 0
        with ready = nil
        while t
        do (loop with rlen = (length (car lines))
                 with clen = (length lines)
                 for r from 0 to (1- rlen)
                 do (loop for c from 0 to (1- clen)
                          do (-> (getval lines r c)
                                 (lambda (val)
                                   (when (and (equal val "@")
                                              (-> (count-adj lines r c)
                                                  (< 4)))
                                     (push (list r c) ready))))))
        if (eq ready nil)
          do (return total)
        do (loop for (ar ac) in ready
                 do (incf total)
                 do (setf (nth ac (nth ar lines)) "x"))
        do (setq ready nil)))

(def-suite 04-solution)
(in-suite 04-solution)

(test pt1-test
  (is (= (pt1 "test.txt") 13)))

(test pt1-input
  (is (= (pt1 "input.txt") 1437)))

(test pt2-test
  (is (= (pt2 "test.txt") 43)))

(test pt2-input
  (is (= (pt2 "input.txt") 8765)))

(defun main ()
  (run! '04-solution))

#+ciel
(main)
