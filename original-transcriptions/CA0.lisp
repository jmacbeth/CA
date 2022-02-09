(in-package :cdparser)

;;; This is the file CA0.lisp; it contains utility functions used throughout the conceptual analyzer
;;; This is very useful to avoid problems in functions due to some CDs being atomized and others not


(defmacro atom-eval (x)
  `(if (symbolp ,x)
       (symbol-value ,x)
       ,x))


;;; all flags are placed on the list :flags; the following functions test and remove flags

(defparameter *flags* nil)

(defmacro flagon (flag) `(member ,flag *flags*))

(defmacro remove-flag (flag)
  `(setf *flags* (remove ,flag *flags*)))

(defmacro add-flag (flag)
  `(pushnew flag *flags*))

;;; These produce unique atoms of the appropriate type and do the required book-keeping

(defparameter *all-cons* nil)
(defparameter *all-lexes* nil)
(defparameter *all-reqs* nil)
(defparameter *all-pools* nil)

(defmacro new-con ()
  `(push (gentemp "CON") *all-cons*))

(defmacro new-lex ()
  `(push (gentemp "LEX") *all-lexes*))

(defmacro new-req ()
  `(push (gentemp "REQ") *all-reqs*))

(defmacro new-pool ()
  `(push (gentemp "POOL") *all-pool*))

;;; to save a lot of rewriting later, we'll need this
;;; note that value is second
(defmacro putprop (sym val key)
  `(setf (get ,sym ,key) ,val))

;;; This function is used in writing dictionary entries - the usual form is
;;; (def word
;;;      (atts: adj noun ..)
;;;      (requests: (request ...)*)
;;; for commonlisp, we should replace atts: with :atts, requests: with :requests


(defmacro def (stuff)
  `(let ((wd (car ,stuff)))
     (mapc #'(lambda (thing)
               (setf (get wd (car thing)) (cdr thing)))
           (cdr ,stuff)))


               


  
