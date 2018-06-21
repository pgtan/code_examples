;;; -*- lexical-binding: t -*-
;; Project Euler: Problem 5



(require 'seq)
(let ((factor-sum-list '((2) 2)))
  (cadr (seq-reduce (lambda (f-s-l counter) 
                (let* ((sum (cadr f-s-l))
                       (factor-list (car f-s-l))
                       (ncounter (seq-reduce (lambda (count factor)
                            (if (zerop (% count factor)) (/ count factor) count))
                                             factor-list counter)))
                  (if (= ncounter 1)
                      f-s-l
                    (list (cons ncounter factor-list) (* sum ncounter)))))
              (number-sequence 3 20) factor-sum-list)))

      
      





      
