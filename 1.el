; -*- lexical-binding: t -*-
;;; Project Euler Problem: 1

;;; with math

(defun pgt/series-length (start end)
  "given the beginning and the end of series return the length of
the series"
  (1+ (/ (- end start) start)))

(defun pgt/series-sum (start end)
  "given the beginning and the end of a series return the sum of
the series"
  (/ (* (+ start end) (pgt/series-length start end)) 2))

(defun pgt/series-last (start limit)
  "returns the last number in the series smaller than or equal
the limit"
  (- limit (mod (- limit start) start)))

(defun pgt/euler1 ()
  "solution for problem 1"
  (-  (+ (pgt/series-sum 3 (pgt/series-last 3 999))
	(pgt/series-sum 5 (pgt/series-last 5 999)))
     (pgt/series-sum 15 (pgt/series-last 15 999))))


(pgt/euler1)

;;; without math

(apply #'+
        (delete-dups (append
		      (number-sequence 5 999 5)
		      (number-sequence 3 999 3))))

	       
;; Answer: MjMzMTY4 (bas64 encoded)


