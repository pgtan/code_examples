;; -*- lexical-binding: t -*-
;; Project Euler: Problem 8

(require 'seq)
(defun pgt/euler8 ()
  ""
(let* ((number-string (concat  
	       "73167176531330624919225119674426574742355349194934"
	       "71636269561882670428252483600823257530420752963450"))
       (max-prod 0)
      (digits 13)
      (num-idx (- (length number-string) digits)))
  (dotimes (i num-idx max-prod)
    (let* ((num-str (seq-subseq number-string i (+ i digits)))
	   (this-prod (seq-reduce (lambda (prod num-chr) (* prod (- num-chr 48)))
				  num-str 1)))
      (setq max-prod (max max-prod this-prod))))))

(pgt/euler8)
  
