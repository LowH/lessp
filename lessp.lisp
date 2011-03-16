
(defpackage :lessp
  (:use :cl)
  (:export #:lessp))

(in-package :lessp)

(defvar *types-order*
  '(null character number symbol string vector sequence))

;;  Order predicate

(defgeneric lessp (a b))

(defmethod lessp (a b)
  (dolist (type *types-order*)
    (let ((ta (typep a type))
	  (tb (typep b type)))
      (cond ((and ta tb)
	     (error "Dont know how to compare ~S and ~S" a b))
	    (tb (return-from lessp nil))
	    (ta (return-from lessp t)))))
  (error "Dont know how to compare ~S and ~S" a b))

(defmethod lessp ((a number) (b number))
  (< a b))

(defmethod lessp ((a symbol) (b symbol))
  (or (and (eq (symbol-package a) (symbol-package b))
	   (string< (symbol-name a)
		    (symbol-name b)))
      (string< (package-name (symbol-package a))
	       (package-name (symbol-package b)))))

(defmethod lessp ((a string) (b string))
  (string< a b))

(defmethod lessp ((a sequence) (b sequence))
  (let ((la (length a))
	(lb (length b)))
    (dotimes (i (min la lb))
      (let ((ai (elt a i))
	    (bi (elt b i)))
	(cond ((lessp ai bi) (return-from lessp t))
	      ((lessp bi ai) (return-from lessp nil)))))
    (< la lb)))
