(in-package :cdparser)

;;; CA6.lisp  W.L. Johnson 1079 Revision of Larry Birnbaum's code last changed - M. Burstein 11/8/79

#| IF-FIND is called just like lambda: (if-find <args> <s-expression>). It builds a lambda-expression,
which is applied in turn to the conceptualizations on the C-LIST, from the most recently added to the least, and returns the 
first for which the lambda-expression returns a non-NIL value, else returns NIL.
An embedding check is added to the lambda expression

|#
;;; was a DF - substantially rewritten here (MB)
;;; I'm also guessing that SOME used to return a list of answers, so dropping that

(defmacro IF-FIND (arg &rest body)
  `  ;; (let (( temp nil))
     ;;   (setf temp
           (some #'(lambda (,arg)
                    (and (not (get ,arg :embedded))
                         ,@body))
                 C-LIST))
     ;;      (if temp (car temp))))

;;; MB 1/28/21 added FIND-CON to simplify IF-FIND usage to always use C as the arg
(defmacro FIND-CON (body)
  `(some #'(lambda (C)
             (and (not (get c :embedded))
                  ,@body))
         C-LIST))




;;; Returns T if the gap is filled, NIL if not
(defun IS-FILLED (path con)
  (not (equal (get-role-value path con) '(nil))))

;;; This is used to look ahead. It will be made sensitive to clause boundaries.
(defun CHECK-NEXT-ITEM ()
  (and (not (clause-break-point NEXT-WORD))
       NEXT-WORD))

;;; Check if we're at a break in the input stream (a period)
(defun CLAUSE-BREAK-POINT (word)
  (eq word 'PRD))

;;; Checks to see if word could mark the end of a clause
(defun CLAUSE-POINT (word)
  (member word '(cma prd when so)))

;;; This applies the predicate to OBJ and returns the resulting value.
;;; First looks under TYPE property and then slots; else hack groups, else check CAR of object

(defun feature (obj pred)
  (cond ((get pred :expr)
         (apply pred (list obj)))
        ((group obj)
         (or (feature (get-role-filler '(member) obj) pred)
             (feature (get-role-filler '(member1) obj) pred)
             (feature (get-role-filler '(member2) obj) pred)))
        ((eq (car (atom-eval obj)) pred))
        (t (eq (get (car (atom-eval obj)) :type) pred))))

;;; MB- ADDING HAS-FEATURE to look for one of several features

(defun has-feature (obj preds)
  (some #'(lambda (p) (feature obj p))
        preds))

;;; The following assume C-LIST is normally reverse-ordered (last in if first)

;;; PRECEDES returns a non-NIL value iff C1 precedes C2 on the C-LIST (was built before)

(defun PRECEDES (C1 C2)
  (and (note (eq c1 c2))(member c1 (member c2 C-LIST))))

;;; FOLLOWS returns non-NIL iff C1 follows C2 on the C-LIST
(defun FOLLOWS (c1 C2)
  (and (note (eq c1 c2))(member c2 (member c1 C-LIST))))

;;; IM-FOLLOWS returns non-NIL iff C1 immediately follows C2 on the C-LIST
(defun IM-FOLLOWS (c1 C2)
  (eq c2 (second (member c1 C-LIST))))

;;; Returns a non-NIL value iff X is a puctuation; applied at the lexical level; obsolescent
;; was a DEFPROP??
;; (defprop breakpoint (lambda (x) (memq x '(period qmark))) expr)

(defun BREAKPOINT (x)
  (member x '(period qmark)))


;;; added MB 1/27/21
;;; test if x has the right part of speech
(defun has-pos (x pos)
  (let ((xpos (get x :pos)))
     (cond ((null pos) (null xpos))
           ((consp pos)
            (some #'(lambda (p) (member p xpos)) pos))
           (t (member pos xpos)))))


;;; Check if a word is a verb but not a past participle
;;; Note that this solution is plausible from a speech-processing standpoint, i.e. present tense should default to PPART

(defun NOT-PPART (X)
  (and (has-pos x 'verb) ;; (member 'VERB (get x :pos))y
       (member 'PRESPART (get x :morph)))) 

;;; These don't search correctly *** please note***
;;; check if X is subject of Y (is sensitive to passive)

(defun IS-SUBJ (x y)
  (cond ((member :PASSIVE-FLAG :flags) (follows x y))
        (t (precedes x y))))

;; check if X is object of Y

(defun IS-OBJ (x y)
  (cond ((member :PASSIVE-FLAG :flags)
         (precedes x y))
        (t (follows x y))))

;;; FOLLOWS-R first checks for an item which follows, then for an item which preceds and is unbound

(defun FOLLOWS-R (x y)
  (or (follows x y)
      (and (not (get x :embedded)) (precedes x y))))

;;; PRECEDES-R checks for preceding unbound items
;;; This fn was missing a paren after embedded!!! probably not ever used.
;;; it also doesnt make sense relative to FOLLOWS-R's definition. 
(defun PRECEDES-R (x y)
  (and (not (get x :embedded)) (precedes x y)))


