(in-package :CDPARSER)

;; CA7.lisp 10/79 W.L. Johnson - Most of the predicates for testing conceptualizations

;;; mb adding these to reduce typing below
(defun c-head (x)
  (car (atom-eval x)))

(defun c-type (x)
  (get (c-head x) :type))

;;; Check if an item is a PP
(defun PP (x)
    (eq (c-type x) 'pp))

(defun human (x) (eq (c-head x) 'human))

(defun phys-obj (x) (eq (c-head x) 'phys-obj))

(defun location (x) (eq (c-head x) 'loc))

;;; these were commented out in th origininal code
;;; Check if an item is an act
;; (defun act (x) (or (script x) (cd-act x)))

;;; Check if an item is a state
;; (defun state (x) (equal (car (atom-eval x)) 'state)

;;; Checi if an item is a script
(defun SCRIPT (x)
  (eq (c-type x) 'script))



;;; Check if an item is a concept
(defun CONCEPT (x)
  (let ((head (c-head x)))
    (or (member (c-type x) '(primitive-act script state interp-relation))
        (member head '(lead-to enable))
        (and (get head :form) t))))

(defun CONREL (x)
  (member (c-head x) '(enable conrel result)))

;; kludge
(defun TIME (x)
  (member (c-head x) '(time *yesterday* *today* *tomorrow*)))

(defun PART-OF-DAY (x)
  (member (atom-eval x) '((morning) (afternoon) (evening) (night))))

;;; Next, predicates which are more semantic. These really should be considered memory calls,
;;; and eventually implemented that way

;; check if a group
(defun GROUP (x)
  (equal (get-role-value '(PPCLASS) x) '(GROUP)))

;; check if animate
(defun ANIM (x)
  (member (c-head x) '(human anim higheranim)))

;; check if higher-animate (same)

(defun HI-ANIM (X)
 (let ((thing
         (cond ((eq (c-head x) 'group)
                (c-head (get-role-filler '(member) x)))
               (t (c-head x)))))
     (if (member thing '(higheranim hi-anim human))
         t)))


