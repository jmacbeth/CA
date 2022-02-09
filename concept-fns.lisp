(in-package :cdparser)
;;; concept-fns.lisp was CA5.lisp -  contains the functions for building and manipulating CDs.

#|
BUILD-CD is used to build an atomized CD structure. It takes three arguments:
The first must evaluate to a legal CD conceptualization, namely the one which is to be atomized
The second must evaluate to a list of pairs of the form (PATH CD). The CD will be atomized and placed 
  at the end of the path in the structure built by the first argument.
The third argument must evaluate to a list of pairs of the form (PATH1 PATH2). The CD at the end of PATH1 
  replaces the CD at th e end of path 2 in the structure built by the first argument. If PATH 1 is NIL then 
  the entire concept is returned, so that back-pointers can be set.

|#
;; MB 1/28/21 made second and third args optional

;;; MB 1/28/21 ;; couldn't find BUILD-CON used in the dictionary. I'm going to assume it is equivalent to BUILD-CD
;;; but modify it so the first args do not need to be eval'd to remove quotes -- by adding this macro

(defmacro BUILD-CON (concept &optional (fillers nil) (equivalences nil))
  ;;   `(BUILD-CD ',concept ',fillers ',equivalences))
  ;; MB 2/1/21 changing this to call ADD-CON (which calls BUILD-CD) so that
  ;; calls in request actions end up putting structures on C-LIST
  ;;; I don't understand where the disconnect was...concerning
  `(add-con ',concept ',fillers ',equivalences))
            

(defun BUILD-CD (concept &optional (fillers nil) (equivalences nil))
  (let ((con (make-cd concept)))
    (mapc #'(lambda (e) (subst-cd (get-role-filler (car e) con)
                                  (get-role-filler (cadr e) con)
                                  con))
          equivalences)
    (mapc #'(lambda (f)
              (set-role-filler (car f) con (cadr f)))
          fillers)
    con))


;;  mb 1/28/21 in what follows, we should reorder args (path concept filler) -> (concept path filler)


;;; GET-ROLE-VALUE is like GET-ROLE-FILLERS, except that if the role-filler to be returned has been atomized,
;;; it is EVAL'd first.
(defun GET-ROLE-VALUE (path concept)
  (atom-eval (get-role-filler path concept)))

;;; GET-ROLE-FILLER returns the end of the path which it's first argument evaluates to within the CD
;;; which its second argument evaluates to.

(defun GET-ROLE-FILLER (path concept)
  (cond ((null path) concept)
        ((symbolp path)
         (let ((con (atom-eval concept)))
           (and (consp con) (getf (cdr con) path))))
        ((consp path)
         (get-role-filler (cdr path)
                          (get-role-filler (car path) concept)))))


;;; SET-ROLE-FILLER puts an atomized form of its third argument at the end of the path specified by its first argument
;;; within its second argument. It does this by actually replacing the atom, not simply by setting the value.
;;; If slot isn't there, it is added. (changed 11/11/79 MB)
(defun SET-ROLE-FILLER (path concept filler)
  (let ((nc (make-cd filler))
        (oc (get-role-filler path concept)))
    (cond (oc (subst-cd nc oc concept)
             ;; dont have this (chain-gap oc nc)  ;; in preds.lisp (propagage slot change)
              )
          (t (add-gap path concept nc)))
;;    (cond (t (is-running ca)
           (pushnew concept CHANGED-CONS);  ))
    (putprop nc concept :embedded)
    concept))

;;; This adds a gap to a CD at the end of PATH. It only works if all but the last role in PATH already exists, and
;;; the last ROLE is to be added. Filler id FILLER. 

(defun ADD-GAP (path concept filler)  
  (let ((con (get-role-filler (butlast path) concept))
        (role (last path)))
    (cond ((and con (not (get-role-filler role con)))
           (set con (append (atom-eval con)
                            (list (car role) (make-cd filler))))
           role)
          (t nil)))) ;; else an error?

;;; MAKE-CD atomizes a conceptualization unless it already is atomized. It also fires any trigger demons
;;; attached to the new concept.

(defun MAKE-CD (X) ;; was a DEX
  (let ((con (build-c x nil)))
    (cond ((not (eq x con))
           ;;  (fire-demons con (has-value 'DUMMY))
           (and ; (is-running ca)
                (pushnew CON CHANGED-CONS)))) ;; (or (has-value 'DUMMY) CON)
    CON))

;;; BUILD-C and BUILD-M call each other recursively to atomize a CD
(defun BUILD-C (x seen)
  (cond ((atom x) x)
        ((equal x '(previous)) (second seen))
        ((prog (newcon)
            ;; In case somewone passes an XPNed CD, this sets the back-pointer properly
            (setq newcon (new-con))
            (setq seen (cons newcon seen))
            (set newcon (cons (build-m (car x) seen)
                              (build-m (cdr x) seen)))
            (return newcon)))))

(defun BUILD-M (x seen)
  (prog (temp)
     (and (atom x) (return x))
     (setq temp (list nil))
   :loop
     (nconc temp (list (car x) (build-c (second x) seen)))
     (cond ((null (setq x (cddr x))) (return (cdr temp)))
           ((null (cdr x))
            (error  "Bad modifier list ~a " X))
           (t (go :loop)))))

;;; SUBST-CD replaces all occurrences of its second argument with its first argument in its third argument.
;;; it assumes its arguments are atomized CDs.
(defun SUBST-CD (new-c old-c cd)
  (subcdc new-c old-c cd nil))

;;; SUBCDC and SUBCDM call each other recursively to accomplish SUBST-CD
(defun SUBCDC (new-c old-c cd seen)
  (cond ((eq cd old-c) new-c)
        ((member cd seen) cd)
        (t (setq seen (cons cd seen))
           (set cd (cons (subcdm new-c old-c (car (eval cd)) seen)
                         (subcdm new-c old-c (cdr (eval cd)) seen)))
           cd)))

(defun SUBCDM (new-c old-c form seen)
  (prog (temp)
     (cond ((eq form old-c) (return new-c))
           ((atom form) (return form)))
     (setq temp (list nil))
   :loop
     (nconc temp (list (car form) (subcdc new-c old-c (cadr form) seen)))
     (cond ((setq form (cddr form)) (go :loop))
           (t (return (cdr temp))))))


  
            
            
            

  
                
  
  
