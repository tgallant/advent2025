;; advent of code 2025 - day 07
;; https://adventofcode.com/2025/day/7

(in-package :ciel-5am-user)

(defun find-start-pos (row)
  (loop for i from 0 to (1- (length row))
        if (equal "S" (nth i row))
          do (return i)))

(defun find-next-split (rows start)
  (loop with row = (nth 0 start)
        with col = (nth 1 start)
        with len = (1- (length rows))
        for i from row to len
        if (->> (nth i rows)
                (nth col)
                (equal "^"))
          do (-> (list i col)
                 (return))))

(defun split-at-point (point)
  (if (eq point nil) nil
      (bind (((r c) point))
        (list (list r (1- c))
              (list r (1+ c))))))

(defun next-beams (rows beams)
  (loop with result = nil
        with splits = nil
        for beam in beams
        do (-> (find-next-split rows beam)
               (lambda (split)
                 (push split splits)
                 split)
               (split-at-point)
               (lambda (nxt)
                 (when (not (eq nxt nil))
                   (loop for p in nxt
                         if (not (member p result :test #'equal))
                           do (push p result)))))
        finally (return (list splits result))))

(defun pt1 (input)
  (loop with rows = (->> (uiop:read-file-lines input)
                       (mapcar (lambda (line)
                                 (str:split-omit-nulls "" line))))
        with start = (list 0 (find-start-pos (car rows)))
        with beams = (list start)
        with splits = nil
        with done = nil
        while t
        do (-> (next-beams rows beams)
               (lambda (res)
                 (bind (((s n) res))
                   (if (eq n nil)
                       (setq done t)
                       (progn
                         (setq beams n)
                         (setq splits (append splits s)))))))
        if (eq done t)
          do (->> (remove-duplicates splits :test #'equal)
                  (remove nil)
                  (length)
                  (return))))

(def-suite 07-solution)
(in-suite 07-solution)

(test pt1-test
  (is (= (pt1 "test.txt") 21)))

(test pt1-input
  (is (= (pt1 "input.txt") 1490)))

(defun main ()
  (run! '07-solution))

#+ciel
(main)
