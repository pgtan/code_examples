;;; -*- lexical-binding: t -*-
;; Project Euler: Problem 6

(let ((sumq 0)
      (sum 0))
  (dolist (i (number-sequence 1 100) (- (expt sum 2) sumq))
    (setq sumq (+ sumq (expt i 2)))
    (setq sum (+ sum i))))
