(in-package :cdparser)


;;; CA4.lisp -- W.L.Johnson 10/79 revision of Larry Birnbaum's CA4.MLI
;;; 8/79 functions which are used in the actions of requests

;;; This is used in thee actions of requests to spawn new requests.
;;; Request pool is tied to the con atom
;;; a bunch of the below were actually DFs, which have one uneval'd argument - turned into macros here. 

(declaim (special ; NEW-CON ;; (was called !NEW-CON) - rest were keywords -- really just (car C-LIST)
                  EXTRA-REQUESTS
                  LEXICAL-POOL
                  C-LIST
                  LAST-EMBEDDED-CON
                  CHANGED-CONS
                  ))


;; was a DF
(defmacro ACTIVATE (&rest reqs)
  `(activate1 ',reqs))

;; added way to carry bindings from test into reqs being activated in action. 
(defun insert-bindings (reqs bindings)
  (let ((qbdgs (loop for (var . val) in bindings ;; vals must be quoted when inserted
                     collect (cons var (list 'quote val)))))
    (sublis qbdgs reqs)))
        

;;; MB when ACTIVATE called from a REQ, it wasnt making a pool to activate,
;;; we now add reqs to the current pool if called from a req action,
;;; but if no current pool, make a new one. 

(defun ACTIVATE1 (reqs)
  ;; bound in consider
  (declare (special CURRENT-REQ CURRENT-POOL *BINDINGS*))
  ;; need to carry any used bindings into the reqs
  (let* (; (bndreqs (insert-bindings reqs *bindings*))
         ;; use pool name in req symbol to indicate provenance
         (newreqs (make-requests (get CURRENT-REQ :word) reqs *bindings*))  
         (new-con (car C-LIST)))
     (if NEW-CON (setf (get NEW-CON :assoc-reqs) newreqs))
    (pmsg :CA1 t CURRENT-REQ "activating new requests:" newreqs)
    (let ((pool (or CURRENT-POOL (new-pool))))

    (if CURRENT-POOL
        (set CURRENT-POOL (append newreqs (symbol-value CURRENT-POOL)))
        (set pool newreqs)
        )
    (activate-pool pool))))

;; split out this fn to define ADD-NEXT (nee ACTIVATE-IN-NEXT-POOL)
(defun ADD-TO-NEXT-POOL (word reqs &optional (bindings *bindings*))
  (setf EXTRA-REQUESTS (append EXTRA-REQUESTS (make-requests WORD reqs bindings))))


;;; Intended for use mainly in the actions of special requests (those which test for some particular word)
;;; to alter the sense of some other word
(defmacro ADD-TO-WORD-SENSE (&rest reqs)
  `(add-to-next-pool ',WORD ',reqs))

(defmacro ACTIVATE-NEXT (&rest reqs)
  `(add-to-next-pool ',WORD ',reqs *bindings*))

;;; Replaces the definition of the next word with REQS -- not used
#+ignore 
(defmacro CHANGE-NEXT-WORD-SENSE (reqs)
  `(progn (skip-next-word)
          (setf EXTRA-REQUESTS (append EXTRA-REQUESTS (make-requests ,reqs)))))

;;; This is the only way to get a request into the LEXICAL-POOL (AKA SPECIAL-POOL?)
;;; MB added WD arg

;;; this existing fn replaced refs to ACTIVATE-IN-SPECIAL-POOL (which wasnt found) in crash-dic

(defmacro ACTIVATE-LEXICAL-REQS (wd &rest reqs)
  `(let* ((newreqs (make-requests ',WD ',reqs *bindings*)))
     ;(break "activate-lexical")
     (format t "~%Activating lexical requests ~a bindings ~a" newreqs *bindings*)
     (setf LEXICAL-POOL ; (get 'LEXICAL-POOL :requests)
           (append newreqs LEXICAL-POOL)))) ; (get 'LEXICAL-POOL :requests)

;;; ACTIVATE-IN-NEXT-POOL was also referenced but not found in crash-dic
;;; similar to ACTIVATE-LEXICAL-REQS, it put requests into the 'special pool' for inclusion in the next pool



;;; In addition to killing specifically named requests, it can take as special arguments the atoms SELF and REST-OF-POOL
;;; REQUEST is a variable local to consider but free here; its value is the name of the request who's actions
;;; are presently being evaluated. POOL is a variable local to CONSIDER-POOL but free here; its value is the name
;;; of the request pool currently being considered.

(defmacro KILL (reqs)
  `(mapc #'kill-req ,reqs))

(defmacro KILL-SELF ()
  `(kill-req CURRENT-REQ))

(defun kill-req (r)
  (cond ; ((eq r 'self) (setf (get request :active) nil)) ;; not sure what request this would mean. no global called REQUEST
        #+ignore ;; pool was unbound and rest-of-pool not found in this code. 
        ((eq r 'rest-of-pool)
         (mapc #'(lambda (x) (setf (get x :active) nil))
               (get pool :requests)))
        (t (setf (get r :active) nil))))

;;; ADD-DUMMY is used to build an atomized dummy structuure and place it on the C-LIST.
;;; The structure can then be used as an argument to PRECEEDS and FOLLOWS, and also ADD-CON.
;;; C-LIST is often treated as a stack, so the most recent item is at the front.
;;; (changed MB 11/8/79)

(defun ADD-DUMMY (con)
  (let ((con (build-cd '(nil) nil nil)))
    (push con C-LIST)
    con))

;;; KILL-DUMMY deletes a dummy which was previously created for marking purposes.

(defun KILL-DUMMY (dummy)
    (setf C-LIST (remove dummy C-LIST)))

;;; ADD-CON is used to build an atomized CD structure and place it on the C-LIST; it takes four optional args:
;;; The first must evaluate to a legal CD conceptualization, namely the one whic is to be atomized and placed on the C-LIST
;;; The second must evaluate to a list of pairs of the form (PATH CD);
;;;   the CD will be atomized and placed at the end of the path in the structure built by the first argument.
;;; The third argument must evaluate to a list of pairs of the form  (PATH1 PATH2);
;;;   the CD  at the end of PATH1 replqces the CD at the end of PATH2 in the structure built by the first argument.
;;; If there is no fourth argument, then the new CD is placed on the end (= front) of the C-LIST
;;; If there is a fourth argument, then it must evaluate to a dummy marker on the C-LIST, which gets its
;;;   value set to the value of the new CD. The value returned is the name of the atom on the C-LIST

;;; NOTE: (change by MB 11/8/79)
;;; C-LIST is usu. searched in "most recent first" order, so the conceptual "end" of the C-LIST is actually
;;; the front of the list.

;;; NOTE: MB 1/26/2021  This was actually a DFX form - not sure what the X meant.

#+ignore
(defmacro ADD-CON (x) ;; was a DFX
  `(add-con1 ,@x) ;; x was set of up to 4 forms to be eval'd
  )

(defun add-con (&optional concept fillers equivalences marker)
  (declare (special C-LIST))
  (let ((newcon nil))
    (setf newcon (build-cd concept fillers equivalences))
    (cond (marker
           (setf marker (eval newcon)) ;; car c-list was NEW-CON
           marker)
          (t (setf C-LIST (cons newcon C-LIST))
             (format t "~%Adding ~a = ~s" newcon (symbol-value newcon)) ; ~% C-LIST = ~a c-list
             newcon))))

;;; FILL-GAP puts FILLER at the end of PATH in CD.
;;; Since this is usually called when FILLER has been found on the C-LIST, one of the functions of FILL-GAP
;;; is to remove FILLER from the C-LIST, unless the item is important to memory.
;;; Items are no longer removed from the C-LIST, just marked EMBEDDED (MB 11/9)

(defun FILL-GAP (path cd filler)
  (if (or (null path)(null cd)) (return-from fill-gap nil)) ;; added MB
  (unless (consp path) (setf path (list path)))             ;; added MB
  (set-gap path cd filler) ; adds path if needed
  (pmsg :CA1 T "Inserting" filler "into" CD "at" path)
  ;; save the subject of focus of the clause on the act built
  (cond ((and (concept-p cd) (precedes filler cd))
         (setf (get cd :contopic) (realcon filler))  ;; was put not putprop
         (setf LAST-EMBEDDED-CON filler)
         ;; assume memory wants to know about everything. (see change above)
         ;;; there was deleted code here
         ; (PMSG :CA1 t "Embedding" filler "in" CD "at" path)
         (setf (get filler :embedded) cd)
         ))
    cd
    )

;;; COPY-GAP embeds a copy of the filler in the slot
(defun COPY-GAP (path cd filler)
  (let ((newcon (add-con (atom-eval filler))))
    (setf (get newcon :realcon) filler)
    (fill-gap path cd newcon)))

;;; SET-GAP puts FILLER at the end of PATH in CD & updates corresponding memory token (if any)
(defun SET-GAP (path cd filler)
  (set-role-filler path cd filler)
;; not used here - we don't have that code
;;  (fill-mem-gaps path cd filler) ; fill corresponding gaps in memory tokens (&goals,plans) see preds.lisp 
  )

;;; MERGE-CONS takes two conceptualizations on the C-LIST and creates a new one consisting of the union
;;; their of semantic content 

(defun MERGE-CONS (con1 con2)
  (pmsg :merge-cons t "Merging" con2 "into" con1)
  (set con1 (append (atom-eval con1) (cdr (atom-eval con2))))
  (setf CHANGED-CONS (remove con2 CHANGED-CONS))
  (setf C-LIST (remove con2 C-LIST)))

  


              
;;; CA6.lisp  W.L. Johnson 1079 Revision of Larry Birnbaum's code last changed - M. Burstein 11/8/79

#| IF-FIND is called just like lambda: (if-find <args> <s-expression>). It builds a lambda-expression,
which is applied in turn to the conceptualizations on the C-LIST, from the most recently added to the least, and returns the 
first for which the lambda-expression returns a non-NIL value, else returns NIL.
An embedding check is added to the lambda expression

|#
;;; was a DF - substantially rewritten here. 
;;;; NEW (MB) - modified to remove extra lambda-ness in surface definition forms
;;;; by always using C as arg, splicing in body directly

(defmacro IF-FIND (&rest body) ;; removed first arg ARG, 
  `(let ((temp nil))
     (setf temp
           (find-if #'(lambda (C) ;; changed (,var) to C
                    (and (not (get C :embedded))
                         ,@body))
                 C-LIST))))



;;; Returns T if the gap is filled, NIL if not
(defun IS-FILLED (path con)
  (not (equal (get-role-value path con) '(nil))))

;;; This is used to look ahead. It will be made sensitive to clause boundaries.
(defun CHECK-NEXT-ITEM ()
  (and (not (clause-break-point)) ;; 
       (next-word)))

;;; Check if we're at a break in the input stream (a period)
(defun CLAUSE-BREAK-POINT () ;; took arg, called with NEXT-WORD above (only call)
  (let ((wd (next-word)))
    (or (eq wd 'PRD)
        (null sentence) ;; if they didnt bother to put a period
        )))

;;; Checks to see if word could mark the end of a clause
(defun CLAUSE-POINT ()
  (let ((wd (next-word)))
    (or (null wd)
         (eq wd 'PRD) ;; (member wd '(cma prd when so)) ;; don't stop on clauses - do whole sentence
         )))

;;; This applies the predicate to OBJ and returns the resulting value.
;;; First looks under TYPE property and then slots; else hack groups, else check CAR of object

;; changed to allow a list of features - do an or

(defun feature (obj pred)
  (cond ((consp pred) (some #'(lambda (p) (feature obj p)) pred))
        ((and (symbolp pred) (get pred :expr))
         (apply pred (list obj)))
        ((eq pred 'concept) (concept-p obj))
        ((group obj)
         (or (feature (get-role-filler :member obj) pred)
             (feature (get-role-filler :member1 obj) pred)
             (feature (get-role-filler :member2 obj) pred)))
        ((eq (c-head obj) pred))
        ((eq (get (c-head obj) :type) pred))
        ((eq (car (c-prop obj :class)) pred))
        ((eq (car (c-prop obj :type)) pred))))


;;; The following assume C-LIST is normally reverse-ordered (last in if first)

;;; PRECEDES returns a non-NIL value iff C1 precedes C2 on the C-LIST (was built before)

(defun PRECEDES (C1 C2)
  (and (not (eq c1 c2))
       (member c1 (member c2 C-LIST))))

;;; FOLLOWS returns non-NIL iff C1 follows C2 on the C-LIST
(defun FOLLOWS (c1 C2)
  (and (not (eq c1 c2))
       (member c2 (member c1 C-LIST))))

;;; IM-FOLLOWS returns non-NIL iff C1 immediately follows C2 on the C-LIST
(defun IM-FOLLOWS (c1 C2)
  (eq c2 (second (member c1 C-LIST))))

;;; Returns a non-NIL value iff X is a puctuation; applied at the lexical level; obsolescent
;; was a DEFPROP??
;; (defprop breakpoint (lambda (x) (member x '(period qmark))) expr)

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
  (and (has-pos x 'verb) ;; (member 'VERB (get x :pos))
       (member 'PRESPART (get x :morph)))) 

;;; These don't search correctly *** please note***
;;; check if X is subject of Y (is sensitive to passive)

(defun IS-SUBJ (x y)
  (cond ((flagon :PASSIVE-FLAG) (follows x y))
        (t (precedes x y))))

;; check if X is object of Y

(defun IS-OBJ (x y)
  (cond ((flagon :PASSIVE-FLAG) (precedes x y))
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


