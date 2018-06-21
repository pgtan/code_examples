; -*- lexical-binding: t -*-
;;; Project Euler Plroblem: 2

(require 'ring)

(defun pgt/fib-next (ring)
  "calculate the next Fib. number and push it into the ring"
  (ring-insert ring (apply #'+ (ring-elements ring))))

(defun pgt/sum-fib-even (n)
  "non-recursive fibonacci calculation using rings"
  (let ((r (make-ring 2))
	(sum-even-numbers 0))
    (dotimes (i 2) (ring-insert r 1)) 	; seed the ring with the first  Fib. numbers
    (while (<= (ring-ref r 0) n)
      (pgt/fib-next r) 
      (setq sum-even-numbers (+ sum-even-numbers (ring-ref r 0)))
      (dotimes (i 2) (pgt/fib-next r)))
     sum-even-numbers))

(pgt/sum-fib-even 4000000)




