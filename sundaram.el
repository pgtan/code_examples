;; -*- lexical-binding: t -*-
;; Sieve of Sundaram

(require 'seq)

(defun pgt/gen-jlist (n i)
  ""
  (number-sequence i (floor (/ (- n i) (1+ (* 2 i))))))

(defun pgt/gen-i-j (i j n)
  ""
  (let ((num (+ i j (* 2 i j))))
    (when (<= num n) num)))

(defun pgt/primes (bool-vec)
  ""
  (delq nil (seq-map-indexed (lambda (elt idx) (when elt (1- idx))) bool-vec)))

(defun pgt/sieve-sundaram (n)
  "sieve of sundaram"
  (let ((is-prime (vconcat [nil nil] (make-bool-vector n t)))
	(ilist (number-sequence 1 (floor (/ (1- (sqrt (1+ (* 2 n)))) 2)))))
    (dolist (inum ilist (mapcar (lambda (x) (1+ (* 2 x)))
				(pgt/primes is-prime)))
      (let ((jlist (pgt/gen-jlist n inum)))
	(dolist (jnum jlist)
	  (let ((noprime (pgt/gen-i-j inum jnum n)))
	    (when noprime (aset is-prime (1+ noprime) nil))))))))


