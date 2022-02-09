(in-package :cdparser)

;;; This is the file CA0.lisp; it contains utility functions used throughout the conceptual analyzer
;;; This is very useful to avoid problems in functions due to some CDs being atomized and others not


(defmacro atom-eval (x)
  `(if (symbolp ,x)
       (symbol-value ,x)
       ,x))



;;; These produce unique atoms of the appropriate type and do the required book-keeping

(defun new-con ()
  (car (push (gentemp "CON") ALL-CONS)))

(defun new-lex (word)
  (car (push (gentemp (format nil "LEX-~a-" word)) ALL-LEXES))) ;; MB: added word to symbols 

(defun new-req (wd)
  (car (push (gentemp (format nil "REQ-~a-" wd)) ALL-REQS)))

(defun new-pool (&optional word)
  (car (push (gentemp (format nil "POOL-~a-" word) ) ALL-POOLS)))

;;; to save a lot of rewriting later, we'll need this
;;; note that value is second
(defmacro putprop (sym val key)
  `(setf (get ,sym ,key) ,val))

(defmacro neq (arg1 arg2)
  `(not (eq ,arg1 ,arg2)))



;;; =====CONTROLS on PMSG output ==============================================

(defparameter *pmsg-flags* T) ;; show all msgs if T, else a list of keywords

(defmacro PMSG (&rest args)
  `(when (and *pmsg-flags*
              (or (eq *pmsg-flags* t)
                  (eq t (first ,args))
                  (member (first ,args) *pmsg-flags*)))
     (pmsg1 ,@(cdr args))))

(defun pmsg1 (&rest args)
     (loop for arg in args ; in (mapcar #'eval ',(cdr args))
           with nl = t
           do (unless nl (princ " "))
              (setf nl nil)
              (cond ((eq arg t) (setf nl t) (terpri))
                    ((stringp arg) (princ arg))
                    (t (prin1 arg)))))    



;;; =====CONTROLS on request tracing ==========================================

(defparameter *trace-reqs* nil)

(defmacro trace-reqs (&rest reqs)
  `(loop for wd in ',reqs
         do (pushnew *trace-reqs* wd)))

(defmacro untrace-reqs (&rest (reqs nil))
  (cond ((null reqs) `(setf *trace-reqs* nil))
        (t `(loop for r in ',reqs
                  do (setf *trace-reqs*
                           (remove r *trace-reqs*))))))


;;; ==========utils for MB debugging===========================================
  
(defmacro pool-reqs (pool) ;; pool is a symbol
  `(symbol-value ,pool))

(defmacro props (s)
  `(symbol-plist ',s))
                 
