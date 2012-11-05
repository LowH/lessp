;;
;;  lessp  -  Generic order predicate
;;
;;  Copyright 2011,2012 Thomas de Grivel <billitch@gmail.com>
;;
;;  Permission to use, copy, modify, and distribute this software for any
;;  purpose with or without fee is hereby granted, provided that the above
;;  copyright notice and this permission notice appear in all copies.
;;
;;  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;;  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;;  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;;  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;;  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;;  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;;  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
;;

(defpackage :lessp
  (:use :cl)
  (:export #:lessp #:lessp-equal #:equal-from-lessp))

(in-package :lessp)

(defvar *types-order*
  (list 'null
	'character
	'number
	'package
	'symbol
	'string
	'vector
	'pathname))

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

(defmethod lessp ((a fixnum) (b fixnum))
  (< a b))

(defmethod lessp ((a number) (b number))
  (< a b))

(defmethod lessp ((a package) (b package))
  (and b
       (or (null a)
	   (lessp (package-name a)
		  (package-name b)))))

(defmethod lessp ((a symbol) (b symbol))
  (and b
       (or (and (eq (symbol-package a) (symbol-package b))
		(string< (symbol-name a)
			 (symbol-name b)))
	   (lessp (symbol-package a)
		  (symbol-package b)))))

(defmethod lessp ((a string) (b string))
  (string< a b))

(defmethod lessp ((a cons) (b cons))
  (cond ((lessp (car a) (car b)) t)
	((lessp (car b) (car a)) nil)
	(t (lessp (cdr a) (cdr b)))))

(defmethod lessp ((a vector) (b vector))
  (let ((la (length a))
	(lb (length b)))
    (dotimes (i (min la lb))
      (let ((ai (elt a i))
	    (bi (elt b i)))
	(cond ((lessp ai bi) (return-from lessp t))
	      ((lessp bi ai) (return-from lessp nil)))))
    (< la lb)))

(defun pathname-string (p)
  (declare (type pathname p))
  (with-output-to-string (s)
    (print-object p s)))

(defmethod lessp ((a pathname) (b pathname))
  (lessp (pathname-string a) (pathname-string b)))

;;  Equal

(defgeneric lessp-equal (a b))

(defmethod lessp-equal (a b)
  (not (or (lessp a b)
	   (lessp b a))))

;;  For derived lessp

(defun equal-from-lessp (lessp)
  (lambda (a b)
    (not (or (funcall lessp a b)
	     (funcall lessp b a)))))

(define-compiler-macro equal-from-lessp (lessp)
  (typecase lessp
    ((or function symbol) `(lambda (a b)
			     (not (or (,lessp a b)
				      (,lessp b a)))))
    (t `(equal-from-lessp ,lessp))))
