(in-package :CDPARSER)

;; CA7.lisp 10/79 W.L. Johnson - Most of the predicates for testing conceptualizations

;;; mb adding these to reduce typing below
(defun c-head (x)
  (let ((form (atom-eval x)))
    (and (consp form) (car form))))

;; prop can be a role path
(defun c-prop (x prop)
  (let* ((target  (get-role-filler prop x)))
    (and target (atom-eval target))))



;; check if a group
(defun GROUP (x)
  (equal (get-role-value :CLASS x) '(GROUP)))

;;; handles type as a set of types, and also handles c if a group with a member of type typ
(defun has-type (c typ)
  (let ((head (c-head c))
        (member nil)
        )
    (cond ((consp typ)
           (some #'(lambda (t1) (has-type c t1)) typ))
          (t 
           (or (eq head typ)
               (eq (get head :type) typ)
               (and (group c)
                    (setf member (get-role-filler :member c))
                    (has-type member typ)))))))
    



;;; Check if an item is a PP
(defun PP (x) (has-type x '(*pp* pp)))
(defun human (x) (has-type x 'human))
(defun phys-obj (x) (has-type x 'phys-obj))
(defun location (x) (has-type x '(*loc* location loc)))



;;; this is added by MB 1/29/21
(eval-when (:load-toplevel :compile-toplevel :execute)
  (loop for (head typ) in
        '((*pp* pp) ;; I don't remember what PP stood for, but it tends to be from a noun phrase
          (*loc* loc)
          (*mtrans* primitive-act)
          (*ptrans* primitive-act)
          (*atrans* primitive-act)
          (*propel* primitive-act)
          (*mbuild* primitive-act)
          (*ingest* primitive-act)
          (*grasp* primitive-act)
          (*attend* primitive-act)
          (*speak* primitive-act)
          (*move* primitive-act)
          (*expel* primitive-act)
          )
        do (setf (get head :type) typ)))

 


;;; these were commented out in th origininal code
;;; Check if an item is an act
;; (defun act (x) (or (script x) (cd-act x)))

;;; Check if an item is a state
;; (defun state (x) (equal (car (atom-eval x)) 'state)

;;; Checi if an item is a script
(defun SCRIPT (x)
  ; (or 
   ; (eq (c-type x) 'script)
   (let* ((h (c-head x))
          (hname (if (symbolp h)
                      (symbol-name h))))
     (and hname 
          ; (princ hname)
          (char= (elt hname 0) #\$))))
; )
  



;;; Check if an item is a concept
(defun CONCEPT-P (x)
  (let* ((form (atom-eval x))
         (head (c-head form)))
    (or (has-type x '(primitive-act state interp-relation))
        (script x)
        (member head '(lead-to enable *do*))
        (and (symbolp head) (get head :form) t))))

(defun CONREL-P (x)
  (member (c-head x) '(enable conrel result)))

;; kludge
(defun TIME-P (x)
  (member (c-head x) '(time yesterday today tomorrow)))

(defun PART-OF-DAY-P (x)
  (member (atom-eval x) '((morning) (afternoon) (evening) (night))))

;;; Next, predicates which are more semantic. These really should be considered memory calls,
;;; and eventually implemented that way


;; check if animate
(defun ANIM-P (x)
  (member (c-head x) '(human anim higheranim)))

;; check if higher-animate (same)

(defun HI-ANIM-P (X)
  (has-type x '(higheranim hi-anim human)))
#+ignore
 (let ((thing
         (cond ((eq (c-head x) 'group)
                (c-head (get-role-filler :member x)))
               (t (c-head x)))))
     (if (member thing '(higheranim hi-anim human))
         t))


