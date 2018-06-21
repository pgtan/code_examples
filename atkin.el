;; -*- lexical-binding: t -*-
;; sieve of Atkin

(require 'generator)
(require 'calc-comb)

(defvar pgt/s1 '(1 13 17 29 37 41 49 53))
(defvar pgt/s2 '(7 19 31 43))
(defvar pgt/s3 '(11 23 47 59))
(defvar pgt/s  '(1 7 11 13 17 19 23 29 31 37 41 43 47 49 53 59))



;; Data abstraction

(defun pgt/make-prime (num)
  ""
  (cons num t))

(defun pgt/make-nonprime (num)
  ""
  (cons num nil))

(defun pgt/flip-prime (prime)
  ""
  (cons (car prime) (not (cdr prime))))

(defun pgt/primep (prime)
  ""
  (cdr prime))

(defun pgt/make-primes (&rest primes)
  ""
  (append primes))

(defun pgt/concat-primes (&rest primes)
  ""
  (apply #'append primes))

(defun pgt/add-prime (prime primes)
  ""
  (push prime primes))

(defun pgt/delete-prime (prime primes)
  ""
  (delete prime primes))

  
(defun pgt/find-prime-num (num primes)
  ""
  (assoc num primes))

(defun pgt/get-prime-num (prime)
  ""
  (car prime))

(defun pgt/first-nonprime (primes)
  ""
  (rassoc nil primes))

(defun pgt/first-prime (primes)
  ""
  (rassoc t primes))

(defun pgt/sort-two-primes (prime1 prime2)
  "t if prime1 is smaller than prime2"
  (< (pgt/get-prime-num prime1) (pgt/get-prime-num prime2)))


;; Generators
(iter-defun pgt/gen-num (num)
  ""
  (let ((i (1- num)))
    (while t (iter-yield (setq i (1+ i))))))


(iter-defun pgt/gen-odd ()
  ""
  (let ((i -1))
    (while t (iter-yield (setq i (+ 2 i))))))


(iter-defun pgt/gen-even ()
  ""
  (let ((i 0))
    (while t (iter-yield (setq i (+ 2 i))))))

;; Init

(defun pgt/seed-sieve (limit s)
  ""
  (apply #'append (mapcar (lambda (x) (mapcar (lambda (i)  
				(pgt/make-nonprime (+ (* 60 x) i))) s))
	  (number-sequence 0 (/ limit 60)))))

;; Step 3.1
(defun pgt/step31 (seed limit)
  ""
  (let ((x-gen (pgt/gen-num 1))
	(dlimit (sqrt limit))
	(primes-candidates (pgt/make-primes)))
    (while (< (setq x (iter-next x-gen)) dlimit)
      (let ((y-gen (pgt/gen-odd)))
	(while (< (setq y (iter-next y-gen)) dlimit)
	  (let ((n (+ (* (expt x 2) 4) (expt y 2))))
	    (when (and (<= n limit) (member (% n 60) pgt/s1) (pgt/find-prime-num n seed))
	      (setq primes-candidates (pgt/add-prime (pgt/make-prime n) primes-candidates)))))))
      primes-candidates))


;; Step 3.2
(defun pgt/step32 (seed limit)
  ""
  (let ((x-gen (pgt/gen-odd))
	(dlimit (sqrt limit))
	(primes-candidates (pgt/make-primes)))
    (while (< (setq x (iter-next x-gen)) dlimit)
      	(let ((y-gen (pgt/gen-even)))
	  (while (< (setq y (iter-next y-gen)) dlimit)
	    (let ((n (+ (* (expt x 2) 3) (expt y 2))))
	      (when (and (<= n limit) (member (% n 60) pgt/s2) (pgt/find-prime-num n seed))
	    (setq primes-candidates (pgt/add-prime (pgt/make-prime n) primes-candidates)))))))
    primes-candidates))


;; Step 3.3
(defun pgt/step33 (seed limit)
  ""
  (let ((x-gen (pgt/gen-num 2))
	(dlimit (sqrt limit))
	(primes-candidates (pgt/make-primes)))
    (while (< (setq x (iter-next x-gen)) dlimit)
      (let ((y-gen (pgt/gen-odd)))
      (while (> (setq y (- x (iter-next y-gen))) 0)
	(let ((n (- (* (expt x 2) 3) (expt y 2))))
	  (when (and (<= n limit) (member (% n 60) pgt/s3) (pgt/find-prime-num n seed))
	    (setq primes-candidates (pgt/add-prime (pgt/make-prime n) primes-candidates)))))))
    primes-candidates))

(defun pgt/sieve-atkin (limit)
  "" 
  (let* ((seed (pgt/seed-sieve limit pgt/s))
	 (prim-cand (sort (pgt/concat-primes (pgt/step31 seed limit)
					     (pgt/step32 seed limit)
					     (pgt/step33 seed limit))
			  #'pgt/sort-two-primes))
	 (w (pgt/gen-num 0)))
    (dolist (x pgt/s)
      (while (< (expt (setq n (*  (+ x (* 60 (iter-next w))))) 2)  limit)
	(when (pgt/find-prime-num n prim-cand)
	  (dolist (x1 pgt/s)
	    (let ((w1 0))
	      (while (< (setq c (*  (expt n 2) (+ x1 (* 60 (setq w1 (1+ w1))))))  limit)
		(setq prim-cand (pgt/delete-prime (pgt/make-prime c) prim-cand))))))))
    (delete-dups prim-cand)))


(defun pgt/primes (x)
  "probe all primes from the calc package"
  (let ((n x)
	(primes ()))
    (dolist (i (mapcar #'identity math-primes-table))
      (while (zerop (% n i))
	(push i primes)
	(setq n (/ n i))))
    (if (> n 1)
	n
      (- (car primes)))))


(defun pgt/euler3-atkin (num)
  "use already known primes to lower the range, then generate all primes with the sieve of atkin and test 
the rest of the primes for divisibility"
  (let* ((numb (pgt/primes num))
	 (n (1+ numb)))
    (cond
     ((< numb 0) (- numb)) 		                               ; found in the first primes
     ((and (zerop (% num numb)) (< numb num)) (pgt/euler3-atkin numb)) ; go with the smaller number
     (t					                               ; the whole catastrophe
      (let* ((last-prime (car (last (mapcar #'identity math-primes-table))))
	     (primes (pgt/sieve-atkin  n)))
	(catch 'found (dolist (i (nreverse (mapcar #'pgt/get-prime-num primes)))
			(when  (> i last-prime)
			  (when (zerop (% num i))
			    (throw 'found i))))))))))

(pgt/euler3-atkin 60085147514312)



  
  
