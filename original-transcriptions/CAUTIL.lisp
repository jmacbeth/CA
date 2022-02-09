(in-package :cdparser)

;; CAUTIL.lisp 10/79 W.L.Johnson - Utility functions for use with CA

;;; FREECONS returns those objects which are not embedded
(defun FREECONS ()
  (loop for c in C-LIST
        when (not (get c :embedded))
          collect c))

;;; DELINFREE returns C-LIST with embedded cons in parens. It also reverses it so they are in most recent last order
(defun DELINFREE ()
  (let ((cl nil))
    (loop for c in C-LIST
          do (push (if (get c :embedded)
                       (list c)
                       c)
                   cl))
    cl))

;;; ACTIVE-REQS returns list of active requests
(defun ACTIVE-REQS ()
  (let ((reqs ;; was (for (x in REQUEST-POOLS) (SPLICE (COPY (get x 'requests)))) ;; assuming splice = loop append?
          (loop for x in REQUEST-POOLS
                append (copy-list (get x :requests))))) 
    (loop for x in reqs when (get x :active) collect x)))

;;; PRINT-CLIST prints out the conceptualizations
(defun PRINT-CLIST ()
;; was (for (x in (reverse (freecons))) (do (msg t x) (cdprcon (eval x))))
  (loop x in (reverse (freecons))
        do (format t "~%~a - ~a" x (cdprcon (eval x))))) ;; CDPRCON may be missing?

(defun PRINT-REQS ()
;; was (for (x in (active-reqs)) (do (msg t x t) (sprint (get x 'body) 1)))
  (loop for x in (active-reqs)
        do (format t "~%~a~%  ~a" x (get x :body))))

;;was dex
(defun REALCON (con)
  (loop with ncon = con
        while (setf ncon (get con :realcon))
        do (setf con ncon)
        finally (return con)))

;;; All but last elt of a list
(defun BUTLAST (x)
  (ldiff x (last x)))

