(in-package :cdparser)

;; UTIL.lisp was CAUTIL.lisp 10/79 W.L.Johnson - Utility functions for use with CA

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
          (loop for p in REQUEST-POOLS
                append (copy-list (pool-reqs p)))))
    (loop for req in reqs when (get req :active) collect req)))

;;; PRINT-CLIST prints out the conceptualizations
(defun PRINT-CLIST ()
;; was (for (x in (reverse (freecons))) (do (msg t x) (cdprcon (eval x))))
  (loop for x in (reverse (freecons))
        do (format t "~%~a - ~a" x (xcon x)))) ;; CDPRCON may be missing?

(defun XCON (x &optional (seen nil))
  (cond ((consp x)
         (mapcar #'(lambda (y) (xcon y seen)) x))
        ((and (symbolp x) (member x seen))  x)
        ((and (symbolp x)(boundp x) (symbol-value x))
         (push x seen)
         (xcon (symbol-value x) seen))
        (t x)))
          
(defun XCONS (&optional (clist (delinfree)) (showfree t))
  (loop for c in clist
        unless (and showfree (consp c))
          collect (xcon c)))



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

(defmacro PUSH-END (l e)
  `(setf ,l (append ,l (list ,e))))

;; item is in s-exp tree
(defun in-tree-p (item tree)
  (cond ((equal item tree) item)
        ((not (consp tree)) nil)
        (t (or (in-tree-p item (car tree))
               (in-tree-p item (cdr tree))))))


;; item is head of some sub expression
(defun sub-head-p (item tree)
  (cond ((not (consp tree)) nil)
        ((equal item (car tree)) tree)
        (t (or (sub-head-p item (car tree))
               (sub-head-p item (cdr tree))))))

