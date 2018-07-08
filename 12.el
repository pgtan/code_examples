;; -*- lexical-binding: t -*-
;; Project Euler: Problem 12

(require 'seq)
(eval-when-compile (require 'cl-lib))

;;; Primes

;; Sieve of Sundaram
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


(setq primes-list (pgt/sieve-sundaram 100000))

;; Beyond the sieve

(defun pgt/nprimes-odd-check (n list)
    "probe all primes from the list"
    (let ((primes ()))
      (cl-loop for i in list while (<= i n) finally return (list n (reverse primes)) do
               (while (zerop (% n i))
                 (push i primes)
                 (setq n (/ n i))))))

(defun pgt/nprimes-odd-rest (n list)
    "check the numbers after the last known prime until, but not n. return list of primes"
    (cl-loop for i from (+ 2 (car (last list))) to (- n 2) by 2 finally return newlist
 	     for result = (pgt/nprimes-odd-check i newlist) 
             for rest = (car result)
	     for newlist = list then (if (= rest 1) newlist (append newlist (list rest)))))

(defun pgt/nprimes-odd (n)
  "returns all divisors and updates the primes-list"
  (when (cl-evenp n) (error "Error: %d even" n))
  ;; first we check with our primes list
  (let* ((first-check (pgt/nprimes-odd-check n primes-list))
	 (first-factors (cadr first-check))
	 (first-rest (car first-check)))
    (if (= first-rest 1)
	first-factors
      (setq primes-list (pgt/nprimes-odd-rest first-rest primes-list))
      (let* ((last-check (pgt/nprimes-odd-check first-rest primes-list))
	     (last-rest (car last-check)))
	(cond
	 ((= last-rest 1) first-factors)
	 ((= last-rest first-rest) (setq primes-list (append primes-list (list last-rest)))
				   (append first-factors (list last-rest)))
	 (t (error "This cannot happen %d" last-rest)))))))

(defun pgt/nprimes-even (n)
  "returns a divisor list"
  (cl-loop with rest = n
	   while (cl-evenp rest)
	   count rest into counts
	   finally return (append (make-list counts 2)
				  (when (/= 1 rest) (pgt/nprimes-odd rest)))
	   do (setq rest (/ rest 2))))



;;; Combinations

(defun pgt/comb (lst k)
  "list lst, choose k, all combinations from 1 to k"
  (cl-loop with n = (length lst)
	   for i from 1 to k
	   with numlist = (number-sequence 0 (1- n))
	   for resultlst = (mapcar #'list numlist) then (cl-loop
							 for elmnt in resultlst
							 for lastel = (car (last elmnt))
							 for feedlst = (last numlist (- n (1+ lastel)))
							 append (mapcar (lambda (x)
									  (append elmnt (list x)))
									feedlst))
	   append resultlst into finallst
	   finally return (mapcar (lambda (el)
				    (mapcar (lambda (num)
					      (nth num lst))
					    el))
				  finallst)))

(defun pgt/all-comb (lst)
  (pgt/comb lst (length lst)))


;;; Algorithm:      
;;        n(n + 1) 
;; we use --------  
;;            2    
;;                 
;; 1. Take the divisors of n from the last (n+1) list.
;; 2. If (n+1) is even compute the divisors of (n+1)/2. 
;; 3. If (n+1) is odd compute the divisors. 

(defun e12/series-n (n)
  ""
  (/ (* n (1+ n)) 2))

(defun e12 ()
  ""
  (cl-loop for num from 7 
	   with last-num-divisors = (list 7) 
	   with divisors = nil
	   with divisors-length = 0
	   while (< divisors-length 500)
	   finally return (message "%s" (list (1- num) (e12/series-n (1- num)) (sort divisors #'<)))
	   do
	   (setq divisors
		 (delete-dups
		  (mapcar (lambda (x) (apply #'* x))
			  (pgt/all-comb
			   (append last-num-divisors (setq last-num-divisors
							   (if (cl-oddp (1+ num))
							       (pgt/nprimes-odd (1+ num))
							     ;; we take a 2 away
							     (pgt/nprimes-even (/ (1+ num) 2)))))))))
	   (setq divisors-length (length divisors))))

;;; Execution
(e12)

