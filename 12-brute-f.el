;; -*- lexical-binding: t -*-
(eval-when-compile (require 'cl-lib))
(defun e12/series-n (n)
  ""
  (/ (* n (1+ n)) 2))

(defun e12/brute-force ()
  (cl-loop for n from 1000
	   for trian-num = (e12/series-n n)
	   for divisors = (* 2 (length (delq nil (mapcar (lambda (x)
						      (when (zerop (% trian-num x)) x))
						    (number-sequence 2 (sqrt trian-num))))))
	   while (< divisors 499)
	   finally return (message "%s" trian-num)))

(e12/brute-force)
