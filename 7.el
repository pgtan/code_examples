;; -*- lexical-binding: t -*-
;; Project Euler: Problem 7
;; brute force with memoization

(defvar prime-checked (make-bool-vector 100000000 nil))
(defvar is-prime (make-bool-vector 100000000 nil))

(defun pgt/primep (n)
  "check by trial division"
  (let* ((x 3)
	(idx (floor n 2))
	(checked (aref prime-checked idx)))
    (if checked (aref is-prime idx)
      (aset prime-checked idx t)
      (while (and (> n x) (/= (% n x) 0))
	(setq x (+ 2 x)))
      (when (= n x) (aset is-prime idx t)))))


(defun pgt/euler7 (n)
  "nth prime"
  (let ((pcount 2)
	(ncount 5))
    (while (< pcount n)
      (when (pgt/primep ncount) (setq pcount (1+ pcount)))
      (setq ncount (+ 2 ncount)))
    (- ncount 2)))

(pgt/euler7 10001)


    
       
	
