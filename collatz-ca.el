;; -*- lexical-binding: t -*- 

;; Project Euler 14: find longest Collatz sequence using alternative
;; approach based on the article "Two Cellular Automata for the 3x+1
;; Map" (https://arxiv.org/abs/nlin/0502061) by M. Bruschi

;;; Init

(eval-when-compile (require 'cl-lib))
(defvar collatz-array-size 100000000)
(defvar collatz-memo-array (make-vector collatz-array-size nil))


;;; Helper functions

;; Memoization
(defun collatz-memo-put (num val)
  (when (< num (* 2 collatz-array-size))
    (aset collatz-memo-array (/ num 2) val)))

(defun collatz-memo-get (num)
  (when (< num (* 2 collatz-array-size))
    (aref collatz-memo-array (/ num 2))))

;; Bit manipulations
(defun make-tagged-vec (num)
  "Size is the binary size of NUM plus 3 nulls"
  (make-bool-vector (+ 4 (logb num)) nil))

(defun list-strip-num (lst num)
  "Strip leading and trailing NUMs from LST. Return rest"
  (cl-loop for i on (cl-loop for i on (reverse lst)
			     unless (= (car i) num) return (reverse i))
	   unless (= (car i) num) return i))

(defun bin-list-to-num (lst)
  "Return the number from a zero stripped LST"
  (cl-loop
   for power from 0
   for i in (list-strip-num lst 0)
   sum (* (expt 2 power) i)))

(defun num-bits (num)
  "return the length of NUM in base 2"
  (1+ (logb num)))

(defun bit-on-pos (num pos &optional dir)
  "Return the bit from NUM on POS. If DIR coint from left"
  (logand (lsh num (- (if dir (- (1- (num-bits num)) pos) pos))) 1))

(defun flip-num (num)
  "Mirror the bits of a NUM"
  (cl-loop
   with num-length = (1- (num-bits num))
   for i from num-length downto 0
   for power from 0
   sum (* (expt 2 power) (bit-on-pos num i))))

(defun find-bits (num bitstr)
  "Return the start positions of all BITSTR in NUM"
  (cl-loop
   with step = 1
   with bin-length = (logb num)
   with bitnum = (string-to-number bitstr 2)
   with bitnumlength = (length  bitstr)
   with andbits = (1- (expt 2 bitnumlength))	   ; everything 1
   for shift = (1- bin-length) then (- shift step) ; skip SHIFT positions
   until (< shift 0)
   for i = (and (= bitnum
		   (logand		     ; make everything 0, leave first BITNUMLENGTH bits untouched
		    (lsh num (- shift)) ; shift the NUM
		    andbits))
		(+  (1- bitnumlength) shift))
   if i collect i and do (setq step bitnumlength) else do (setq step 1)))

;; Lists
(defun sublist-greater (lst num)
  (cl-loop for list on lst
	   if (> (car list) num) return list))

(defun build-pairs (list1 list2 pos)
  "LIST1 LIST2 POS"
  (cl-loop
   for leftlist = (sublist-greater list1 pos) then  (sublist-greater leftlist rightpos)
   until (null leftlist)
   for leftpos = (car leftlist)
   for rightlist = (sublist-greater list2 leftpos) then (sublist-greater rightlist leftpos)
   until (null rightlist)
   for rightpos = (car rightlist)
   collect (cons leftpos rightpos)))


;;; Rules
(defun rule1a (num)
  "Return all pos of 00 coutings from left to right"
  (mapcar (lambda (x)
	    (- (num-bits num) x))
	  (find-bits num "00")))

(defun rule1bc (num rule1a-pos)
  "Return list of start end cons"
  (build-pairs (mapcar (lambda (x)
			 (- (+ 2 (logb num)) x))
		       (find-bits num "11"))
	       rule1a-pos (1+ (car rule1a-pos))))

(defun rule2 (num vec)
  "Generate a new number from NUM using the bits from VEC. Return a list of 1 and 0"
  (cl-loop
   for bit-pos from 1 to (1- (length vec))
   for bit = (mod (+ (bit-on-pos num (1- bit-pos) t)
		     (bit-on-pos num bit-pos t)
		     (if (aref vec bit-pos) 1 0)) ; tagged?
		  2)
   collect bit))
  
(defun collatz (num)
  (let* ((vec (make-tagged-vec num))
	 (tnum (lsh (flip-num num) 2))
	 (rule1a-pos (rule1a tnum))
	 (rule1bc-pos (rule1bc tnum rule1a-pos)))
    (dotimes (i  (1+ (car rule1a-pos)))
      (aset vec i t))
    (cl-loop
     for (i . j ) in rule1bc-pos
     do (cl-loop for x from i to j do (aset vec x t)))
    (bin-list-to-num (rule2 tnum vec))))

(defun collatz-seq (num)
  "Count steps all the way down from NUM to 1"
  (let ((memsteps (collatz-memo-get num)))
    (if memsteps memsteps
      (cl-loop for i = num then (collatz i)
	   until (= i 1)
	   count i into steps
	   collect i into nums
	   finally (cl-loop
		    for x downfrom steps
		    for memnum in nums
		    do (collatz-memo-put memnum x)) 
	   finally return steps))))

(defun euler14-ca ()
  (cl-loop
   with steps = 0
   with number = 0
   for i from 500001 to 1000001 by 2
   for reminder = (% i 9)
   if (not (memq reminder '(2 4 5 8))) do (let ((tmp (collatz-seq i)))
					    (when (> tmp steps)
					      (setq steps tmp)
					      (setq number i)))
   finally return (cons number steps)))
(euler14-ca)
