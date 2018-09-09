; -*- lexical-binding: t -*-

;; Project Euler 14, find longest Collatz sequence


(require 'generator)
(eval-when-compile (require 'cl-lib))
(require 'avl-tree)
(require 'url)

;;; Collatz Seq.

;; Vars

(defvar collatz-array-size 100000000)
(defvar collatz-max-list '(0 . 0))
(defvar collatz-memo-array (make-vector collatz-array-size nil))

(defun collatz-compare-function (consa consb)
  (< (car consa) (car consb)))

(defvar collatz-big-val-tree (avl-tree-create 'collatz-compare-function))

;; Init functions

(defun collatz-oeis ()
  (let ((delete-trailing-lines t)
	(url-automatic-caching t)
	(oeis-buffer (url-retrieve-synchronously "https://oeis.org/A006577/b006577.txt")))
    (mapc
     (lambda (str) (let* ((str-list (split-string str))
			  (num (string-to-number (car str-list)))
			  (steps (string-to-number (cadr str-list))))
		     (aset collatz-memo-array num steps)
		     (when (and (> num 5000) (cl-oddp num)) (aset collatz-memo-array (* 2 num) (1+ steps)))))
     (split-string (with-current-buffer oeis-buffer
		    (delete-trailing-whitespace)
		    (goto-char (point-min))
		    (forward-paragraph)
		    (buffer-substring-no-properties (point) (point-max))) "\n" t))))

(defun collatz-init ()
  (dotimes (i 10)
    (let* ((j (+ 14 i))
	   (k (1+ j))
	   (power-num (expt 2 j)))
      (collatz-memo-put power-num j)
      (collatz-memo-put (* 2 power-num) k)
      (when (cl-evenp j)
	(aset collatz-memo-array (/ (1- power-num) 3) k)))))

;; Sequence functions

(defun collatz-next-val (num)
  "Returns the next number in the collatz sequence after NUM"
  (if (cl-oddp num)
      (1+ (* 3 num))
    (/ num 2)))

(iter-defun collatz-gen (strt)
  (let ((i strt))
    (while t (iter-yield
	      (setq i (collatz-next-val i))))))

(defun collatz-get-steps (num)
  "Gets the steps from NUM from the memoization or compute if power of 2"
  (cond
   ((collatz-memo-get num))
   ((= 1 (logcount num)) (collatz-memo-put num (logb num)))))

(defun collatz-seq-steps (num)
  "generates the Collatz Seq. for NUM until reaches num with a
known steps."
  (unless (collatz-get-steps num)
    (cl-loop
     with current-collatz = (collatz-gen num)
     for x iter-by current-collatz
     for found-steps = (collatz-get-steps x)
     while (> x 1)
     until found-steps
     collect x into final-list
     count x into steps
     finally  (iter-close current-collatz)
     (collatz-finalize num (1+ found-steps) steps final-list))))

(defun collatz-finalize (num found-steps steps lst)
  "Memoize NUM and the elts of LST using FOUND-STEPS and STEPS.
Update the max steps value"
  (let ((all-steps (+ found-steps steps)))
    (collatz-memo-put num all-steps)
    (collatz-max num all-steps)
    (when lst (cl-loop
	       for stp downfrom (1- all-steps)
	       for elt in lst
	       if (cl-oddp elt) do (collatz-memo-put (* 2 elt) (1+ stp))
	       do (collatz-memo-put elt stp)))))


(defun collatz-max (num steps)
  "find the max using NUM and STEPS"
  (when (> steps (cdr collatz-max-list))
    (setq collatz-max-list (cons num steps))))

;; Memoization

(defun collatz-memo-get (num)
  (if (< num collatz-array-size)
      (aref collatz-memo-array num)
    (cdr-safe (avl-tree-member collatz-big-val-tree (cons num nil)))))

(defun collatz-memo-put (num val)
  (if (< num collatz-array-size)
      (aset collatz-memo-array num val)
     (avl-tree-enter collatz-big-val-tree (cons num val))))

(defun euler14 ()
  (collatz-oeis)
  (collatz-init)
  (cl-loop
   for i from 500001 to 1000001 by 2
   for reminder = (% i 9)
   finally return (message "%s--%s" collatz-max-list (avl-tree-first collatz-big-val-tree))
   if (not (memq reminder '(2 4 5 8))) do (collatz-seq-steps i)))

(euler14)
