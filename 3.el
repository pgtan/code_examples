;;; -*- lexical-binding: t -*-
;; Project Euler: Problem 3

(defun pgt/trial-div (n)
  "check by trial division"
  (let ((x 3)
	(list ()))
    (while (>= n x)
      (if (/= (% n x) 0)
	  (setq x (+ 2 x))
	(push x list)
	(setq n (/ n x))))
    list))

;; using calc primes

(defun pgt/primes (x)
  "probe all primes from the calc package"
  (let ((n x)
	(primes ()))
    (dolist (i (mapcar #'identity math-primes-table))
      (while (zerop (% n i))
	(push i primes)
	(setq n (/ n i))))
    (car (if (> n 1)
	     (pgt/trial-div n)
	   primes))))

(require 'benchmark)
(insert (format "%s" (benchmark-elapse (pgt/trial-div 600851475143))))
(insert (format "%s" (benchmark-elapse (pgt/primes 600851475143))))





