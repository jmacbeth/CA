(in-package :cdparser)


;;; CA4.lisp -- W.L.Johnson 10/79 revision of Larry Birnbaum's CA4.MLI
;;; 8/79 functions which are used in the actions of requests

;;; This is used in thee actions of requests to spawn new requests.
;;; Request pool is tied to the con atom
;;; a bunch of the below were actually DFs, which have one uneval'd argument - turned into macros here. 

(declaim (special NEW-CON ;; (was called !NEW-CON) - rest were keywords
                  EXTRA-REQUESTS
                  LEXICAL-POOL
                  C-LIST
                  LAST-EMBEDDED-CON
                  CHANGED-CONS
                  ))


(defmacro ACTIVATE (reqs)
  `(let ((req (make-requests ',reqs)))
     (if NEW-CON (setf (get NEW-CON :assoc-reqs) req))
    (activate-pool req)))

;;; Intended for use mainly in the actions of special requests (those which test for some particular word)
;;; to alter the sense of some other word

(defmacro ADD-TO-WORD-SENSE (reqs)
  `(setf EXTRA-REQUESTS (append EXTRA-REQUESTS (make-requests ,reqs))))

;;; Replaces the definition of the next word with REQS
(defmacro CHANGE-NEXT-WORD-SENSE (reqs)
  `(progn (skip-next-word)
          (setf EXTRA-REQUESTS (append EXTRA-REQUESTS (make-requests ,reqs)))))

;;; This is teh only way to get a request into the SPECIAL-POOL
(defmacro ACTIVATE-LEXICAL-REQS (reqs)
  `(setf (get 'LEXICAL-POOL (append (make-requests ,reqs)
                                    (get 'LEXICAL-POOL :requests)))))

;;; In addition to killing specifically named requests, it can take as special arguments the atoms SELF and REST-OF-POOL
;;; REQUEST is a variable local to consider but free here; its value is the name of the request who's actions
;;; are presently being evaluated. POOL is a variable local to CONSIDER-POOL but free here; its value is the name
;;; of the request pool currently being considered.

(defmacro KILL (reqs)
  `(mapc #'kill-req ,reqs))

(defun kill-req (r)
  (cond ((eq r 'self) (setf (get request :active) nil))
        ((eq r 'rest-of-pool)
         (mapc #'(lambda x) (setf (get x :active) nil) (get pool :requests)))
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

(defun KILL-DUMMY 
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

(defmacro ADD-CON (x) ;; was a DFX
  `(add-con1 ,@x) ;; x was set of up to 4 forms to be eval'd
  )

(defun add-con1 (&optional concept path-cd path1-path2 marker)
  (let ((con nil)
        ;; (concept (eval (car x))
        (fillers path-cd) ; (eval (second x))
        (equivalences path1-path2)
        (dummy marker))
    (setf NEW-CON (build-cd concept fillers equivalences))
    (cond (dummy
           (setf dummy (eval NEW-CON))
           dummy)
          (t (setf C-LIST (cons NEW-CON C-LIST))
             NEW-CON))))

;;; FILL-GAP puts FILLER at the end of PATH in CD.
;;; Since this is usually called when FILLER has been found on the C-LIST, one of the functions of FILL-GAP
;;; is to remove FILLER from the C-LIST, unless the item is important to memory.
;;; Items are no longer removed from the C-LIST, just marked EMBEDDED (MB 11/9)

(defun FILL-GAP (path cd filler)
  (set-gap path cd filler) ; adds path if needed
  ;; save the subject of focus of the clause on the act built
  (cond ((and (concept cd) (precedes filler cd))
         (setf (get cd :contopic) (realcon filler))  ;; was put not putprop
         (setf LAST-EMBEDDED-CON filler)
         ;; assume memory wants to know about everything. (see change above)
         ;;; there was deleted code here
         (PMSG 'fill-gap t "Embedding " filler " in " CD)
         (setf (get filler :embedded) cd)
         )))

;;; COPY-GAP embeds a copy of the filler in the slot
(defun COPY-GAP (path cd filler)
  (let ((newcon (add-con (atom-eval filler))))
    (setf (get newcon :realcon) filler)
    (fill-gap path cd newcon)))

;;; SET-GAP puts FILLER at the end of PATH in CD & updates corresponding memory token (if any)
(defun SET-GAP (path cd filler)
  (set-role-filler path cd filler)
  (fill-mem-gaps path cd filler) ; fill corresponding gaps in memory tokens (&goals,plans) see preds.lisp
  )

;;; MERGE-CONS takes two conceptualizations on the C-LIST and creates a new one consisting of the union
;;; their of semantic content 

(defun MERGE-CONS (con1 con2)
  (pmsg 'merge-cons t "Merging " con2 " into " con1)
  (set con1 (append (atom-eval con1) (cdr (atom-eval con2))))
  (setf CHANGED-CONS (remove con2 CHANGED-CONS))
  (setf C-LIST (remove con2 C-LIST)))

  


              
