;; advent of code 2025 - day 06
;; https://adventofcode.com/2025/day/6

(in-package :ciel-5am-user)

(defun parse-op (rows col)
  (loop with maxidx = (1- (length rows))
        with nums = nil
        with op = nil
        for i from 0 to maxidx
        if (= i maxidx)
          do (->> (nth i rows)
                  (nth col)
                  (lambda (val)
                    (match val
                      ("*" #'*)
                      ("+" #'+)))
                  (setq op))
        else
          do (->> (nth i rows)
                  (nth col)
                  (parse-number)
                  (lambda (n)
                    (setq nums (append nums (list n)))))
        finally (return (list op nums))))

(defun pt1 (input)
  (loop with lines = (->> (uiop:read-file-lines input)
                          (mapcar (lambda (line)
                                    (str:split-omit-nulls " " line))))
        with cols = (length (car lines))
        with results = nil
        for i from 0 to (1- cols)
        do (-> (parse-op lines i)
               (lambda (op)
                 (reduce (car op) (cadr op)))
               (push results))
        finally (return (reduce #'+ results))))

(defun pad-nums (nums n)
  (loop with results = nil
        for num in nums
        do (loop with val = num
                 while t
                 if (= (length val) n)
                   do (progn (setq results (append results (list val)))
                             (return))
                 else
                   do (setq val (cons "" val)))
        finally (return results)
        ))

(defun convert-nums (nums)
  (->> (mapcar #'write-to-string nums)
       (mapcar (lambda (s)
                 (str:split-omit-nulls "" s)))
       (lambda (vals)
         (->> (mapcar #'length vals)
              (reduce #'max)
              (pad-nums vals)))
       (apply #'mapcar (lambda (&rest args)
                         (serapeum:string-join args "")))
       (mapcar #'parse-number)))


(defun make-seqs (rows)
  (loop with cols = (length (car rows))
        with ops = (car (last rows))
        with seqs = nil
        with start = nil
        for i from 0 to (1- cols)
        if (match (nth i ops)
             ("+" t)
             ("*" t))
          do (progn
               (if (eq start nil)
                   (setq start i)
                   (progn
                     (setq seqs (append seqs (list (list start (1- i)))))
                     (setq start i))))
        finally (-> (append seqs (list (list start (1- cols))))
                    (return))))

(defun parse-seq (seq)
  (let ((op nil)
        (len (length (car seq))))
    (setq op (match (nth (1- len) (car seq))
               ("*" #'*)
               ("+" #'+)))
    (setf (nth (1- len) (car seq)) " ")
    (->> seq
      (mapcar (lambda (row)
                (serapeum:string-join row "")))
      (lambda (joined)
        (list op
              (->> (subseq joined 0 (1- (length joined)))
                   (mapcar #'str:trim)
                   (mapcar #'parse-number)))))))

(defun collect-seq (rows lo hi)
  (loop with seq = nil
        for i from lo to hi
        do (->> rows
                (mapcar (lambda (row)
                          (nth i row)))
                (lambda (vals)
                  (setq seq (append seq (list vals)))))
        finally (-> (parse-seq seq)
                    (return))))

(defun pt2 (input)
  (loop with lines = (->> (uiop:read-file-lines input)
                          (mapcar (lambda (line)
                                    (str:split "" line))))
        with seqs = (->> (make-seqs lines)
                         (mapcar (lambda (seq)
                                   (bind (((lo hi) seq))
                                     (collect-seq lines lo hi)))))
        with results = nil
        for (op nums) in seqs
        do (-> (reduce op nums)
               (push results))
        finally (return (reduce #'+ results))))

(def-suite 06-solution)
(in-suite 06-solution)

(test pt1-test
  (is (= (pt1 "test.txt") 4277556)))

(test pt1-input
  (is (= (pt1 "input.txt") 4405895212738)))

(test pt2-test
  (is (= (pt2 "test.txt") 3263827)))

(test pt2-input
  (is (= (pt2 "input.txt") 7450962489289)))

(defun main ()
  (run! '06-solution))

#+ciel
(main)
