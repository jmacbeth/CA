;;; CA1.LSP -- contains most of the control structure for revised version of CA
;;; (rewritten in LISP by M. Burstein 11/7/79 from CA1.MLI)

;;; CA is the top-level function



(defun CA ()
    ;; :changed-cons holds concepts changed or added to :C-list during one pass of CA
    (setq CHANGED-CONS nil)
    (setq CURRENT-PHRASE nil)
    (setq CURRENT-MODULE :CA)
    ;; If the current sentence is finished, then get a new one and re-initialize;
    (cond ((NULL SENT) (init-ca)))
    (cond ((NULL SENT) (setq WORKING nil) (return-from ca nil)))
    (pmsg :CA1OPT T "Entering CA:" T "   :C-list  = " (delinfree))
    ;; Set :word and :next-word, and look them up in the dictionary;
   (loop for word = (get-next-item)
          while word
          ;;  CYCLE: (get-next-item) ;; NO OPEN CODED GOTO's!!!
          ;;  (cond ((NULL :word) (return nil)))
          do 
             (pmsg :CA T "Beginning main execution cycle;"
                   T ":word = " :word
                   T "SENT = " SENT
                   T ":C-list = " (delinfree)
                   T ":request-pools = " :request-pools)
             ;; remove request pools which no longer contain active requests
             (clean-up-request-pools)
             (consider-lexical-requests)
             ;;If next work ends noun group, then return to regular node immediately,
             ;; so that next call to consider-requests (below) will consider all requests
             (cond ((and (flagon :noun-group-flag) (end-noun-phrase))
                    (remove-flag :noun-group-flag)
                    (add-flag :results-flag)
                    (pmsg :CA1 T "End of noun group;")))
             ;; activate requests associated with :word;
             (activate-item-requests)
             (consider requests)
             ;; If next word begins a noun group, then begin noun-group mode immediately
             ;; so that the next call to consider-requests (above, but later in the loop)
             ;; will consider only latest requests;
             (cond ((and (not (flagon :noun-group-flag))
                         (setq N-P-RECORD (begin-noun-phrase)))
                    (add-flag :noun-group-flag)
                    (if CHANGED-CONS (pmsg :CA1 T "Begin noun group;"))))

             ;; If any add-Con has built a CD which has a lot of memory stuff hooked onto
             ;; it, or if the next word might mark the end of the clause, then return to
             ;; let memory do its thing;
             ;; (cond (
             when (or (flagon :MEMORY-HOOK-FLAG)
                        (CLAUSE-POINT :NEXT-WORD)
                        (flagon :RESULTS-FLAG))
               do 
                  (remove-flag :MEMORY-HOOK-FLAG)
                  (remove-flag :RESULTS-FLAG)
                  (pmsg :CATOP T "CA processed words: " CURRENT-PHRASE
                          T "  :C-list = " (delinfree)
                          T)
                  (setq CHANGED-CONS (repl-unset-cons CHANGED-CONS))
                  (setq CURRENT-MODULE  nil)
                  (return T)
                  ) ;; end loop     ;;;; no goto's! (T (GO CYCLE:))
   ))
;; )

(declaim (special ALL-REQS ALL-CONS ALL-POOLS ALL-LEXES
                  WORD NEXT-WORD C-LIST REQUEST-POOLS
                  EXTRA-REQUESTS FLAGS N-P-RECORDS LAST-EMBEDDED-CON CHANGED-CONS
                  ;; added
                  SENT SENTENCE
                  LEXICAL-POOL))


;;; Initialize the program
(defun INIT-CA ()
  (init-ca-vars)
  (setf SENTENCE (next-sentence))
  (setf SENT SENTENCE)
  (pmsg :initca T "Init-ca new sentence is " T SENT T)
  (putprop 'lexical-pool nil :requests) ; holds requests looking for specific words
  ;; initialize NEXT-WORD
  (cond ((equal (setf NEXT-WORD (car SENT) '*))
         (add-flag :change-trace-flag)
         (setf SENT (cdr SENT))
         (setf NEXT-WORD (car SENT))))
  (get-lex-info :NEXT-LEX NEXT-WORD) ;; was '!next-lex -- not sure I have this right. 
  (cond ((setf N-P-RECORD (begin-noun-phrase))
         (add-flag :noun-group-flag)
         (pmsg :CA1 T "Begin noun group:"))))


(defun NEXT-SENTENCE () (pop INPUT)) ;; first mention of var INPUT?

;; probably should rewrite this but here it was literally
(defun LIST-ADD (c l)
  (or (member c (eval l)) (setf l (cons c (eval l)))))

;;; This sets WORD, NEXT-WORD and SENT, as well as looking up NEXT-WORD.
;;; The first thing is to check if * flag was set, of so change the pause flag.

(defun get-next-item ()
  (cond ((flagon :change-trace-flag)
         (setf PAUSE (not PAUSE))
         (remove-flag :change-trace-flag))
        )
  (setf WORD NEXT-WORD)
  (and WORD (SET WORD WORD))
  (setf SENT (cdr SENT))
  (cond ((equal (setf NEXT-WORD (car SENT)) '*)
         (add-flag :change-trace-flag)
         (setf SENT (cdr SENT))
         (setf NEXT-WORD (car SENT))))
  (setf LEX NEXT-LEX)
  (get-lex-info :next-lex NEXT-WORD)
  (pmsg :CA1 T "The current word is " WORD )
  (setf CURRENT-PHRASE (nconc CURRENT-PHRASE (list WORD)))
  WORD)
         
         
;;; PUT-FIRST takes an unactivated pool and a request and puts the request at the
;;; beginning of the pool. If the request already exists, it is removed from its previous position.

(defun PUT-FIRST (req pool)
  (cond ((member req (get pool :specs))
         (putprop pool (remove req (get pool :special)) :specs)))
  (putprop pool (cons req (get pool :specs)) :specs))

;;; Removes pools which no longer contain active requests from REQUEST-POOLS
(defun CLEAN-UP-REQUEST-POOLS ()
  (clean-up-special-pools)
  (setf REQUEST-POOLS
        (mapcan #'(lambda (p) (and (live-pool p) (ncons p)))
                REQUEST-POOLS)))

;;; pool still contains some live requests
(defun LIVE-POOL (pool)
  (some #'(lambda (r) (get r :active))
        (get pool :requests)))

;;; returns list of live requests in pool
(defun SAVE-LIVE-REQS (pool)
  (putprop pool
           (mapcan #'lambda (r) (and (get r :active) (ncons r))
                   (get pool :requests))
           :requests))

(defun CLEAN-UP-SPECIAL-POOLS ()
  (save-live-reqs LEXICAL-POOL))

;;; this routine considers the requests with tests which are looking for specific words, which are contained in LEXICAL-POOL
(defun CONSIDER-LEXICAL-REQUESTS ()
  (cond ((get 'LEXICAL-POOL requests)
         (pmsg :ca1 t "Considering lexical requests:")
         (consider-pool 'LEXICAL-POOL))
        (t (pmgs :CA1 T "Considering all requests:")
           (consider-all-requests))))

;; This eventually needs to be more sophisticated, handling the problem of request priority within a given pool.
;;; It returns nil unless at least one of the calls to consider returned T.

(defun CONSIDER-POOL (pool)
  (mapcan #'(lambda (reg) (and (consider reg)) (ncons t))
                    (get pool :requests)))

#| This is a bit changed from the original description. In order to be somewhat more "depth-first",
whenever a request is triggered, instead of continuing down the list of request pools, the program immediately goes
back to the most receent ones again. This is especially important if any new requests were spawned. Note that at 
present, consider-pool does not operate that way, so all the requests in a pool get considered even if one has already
triggered. (obviously, REQUEST-POOLS no longer REVERSEd every time through. - kept as stack (mb)
|#
;; was DEX
(defun CONSIDER-ALL-REQUESTS ()
  (loop while (some #'consider-pool REQUEST-POOLS)))

;;; This only considers requests in the most recent pool, including any requests spawned by them (comments above apply here)
(defun CONSIDER-LATEST-REQUESTS () ;; was DEX
  (let ((latest-pool (car REQUEST-POOLS)))
    (loop while (some #'consider-pool ;; list of pools from latest-pool to front of list
                      (ldiff REQUEST-POOLS
                             (cdr (member LATEST-POOL REQUEST-POOLS)))))))

;;; checks each test-action pair of a request in turn.
;;; if some test true, then associated action is eval'd

(defun CONSIDER (request)
  (let ((NEW-CON nil)) ;; was !NEW-CON
    (cond ((get request :active)
           (some #'(lambda (clause)
                     (cond ((eval-test clause)
                            (pmsg :CA T request " has fired;" T "C-LIST - " (delinfree) t)
                            (putprop request nil :active)
                            (eval-action clause))))
                 (get request :body))
           ;; if request fired, (active prop is nil) then return T
           ;; also, if NO-KILL-FLAG set then reactivate the request
           (cond ((null (get request :active))
                  (cond ((flagon :no-kill-flag)
                         (remove-flag-:no-kill-flag)
                         (putprop request t :active)))
                  t)
                 (t nil))
           ))))

;;; Evaluate the text of a request clause
;;; User of ::= in test is done by a fexpr

(defun EVAL-TEST (cl) (eval (cadar cl)))

;;; ::= handles pseudo-assignments within the tests of requests. It works by changing the value of CLAUSE,
;;; which is local to the consider but free here. Changing CLAUSE appropriately communicates the effect of a pseudo-assignment
;;; to eval-action.  N.B. ::= the function only works when called from eval-test. The appearance of this symbol for 
;;; pseudo-assignment in the actions of a request looks the same but in that case the work is done by eval-action
 
(defmacro ::= (var bnd)  ;; was a DFX!
  `(let ((binding ,bnd)) ;; does the eval
     (cond (binding
            (setf CLAUSE (subst (list 'quote binding) ',var) CLAUSE)
            t)
           (t nil))))

;;; Evaluate the actions in a request clause
;;; Note that if a pseudo-assignment appears in an action, eval-action itself does the work - the function ::= is not called

(defun EVAL-ACTION (cl)
    (loop with action = nil
          for act-list first (cdr (second cl)) ;; cdadr
            then (cdr act-list)
          while act-list
          do (setf action (car act-list))
             (cond ((eq (car action) '::=)
                    (setf act-list (subst (list 'quote (eval (third action))) (second action) act-list))
                    )
                   (t (eval action)))))

;;; Activates requests associated with the current word, if any.
;;; EXTRA-REQUESTS holds requests built by requests in LEXICAL-POOL
;;; for activation in the next request pool built, which is here:

(defun ACTIVATE-ITEM-REQUESTS ()
  (cond ((or EXTRA-REQUESTS (get (get LEX :pool) :specs))
         (activate-pool
          (build-pool
           (append EXTRA-REQUESTS
                   (cond ((flagon :skip-word-flag)
                          (remove-flag :skip-word-flag)
                          nil)
                         (t (make-requests (get (get LEX :pool) :requests))))))
          (setf EXTRA-REQUESTS nil)
          ))))

;;; When a request knows what to do with the next word (by checking get-next-item)
;;; then don't load the definitions under the new word.
;;; (see activate-item-requests)

(defun SKIP-NEXT-WORD () (add-flag :skip-word-flag))

;;; BUILD-POOL takes a dictionary and builds it into an active structure. That means constructing a request pool out of it.

(defun BUILD-POOL (new-requests)
  (let ((pool (new-pool)))
    (putprop pool new-requests :specs)
    pool))

;;; ACTIVATE-POOL takes a pool which has been built using BUILD-POOL and puts it on the lsit of request-pools
(defun ACTIVATE-POOL (pool)
  (push pool REQUEST-POOLS))

;;; Given a list of requests in dictionary format, this puts them into active format
(defun MAKE-REQUSTS (reqs)
  (mapcar #'gen-requst reqs))

;; changes one request from dictionary format to active format
(defun GEN-REQUEST (R)
  (let ((request (new-req)))
    (if (symbolp R) (setf R (symbol-value R))) ;; for named requests
    (setf R (cdr R)) ;; removes word 'request
    (putprop request (clausify R) :body)
    (putprop request T :active)
    request))

;;; clausify filters out non-requst-related information from a lexical specification

(defun CLAUSIFY (x)
  (split-clauses (loop for y in x when (member (car y) '(test action)) collect y)))

;; changes a list of test-action-pairs from dictionary format to internal clause format. (T A T A... ) --> ((T A) (T A)..)
;;;; this was recursive:
#+ignore
(defun split-clauses (x)
  (and x (append (list (list (car x) (cadr x)))
                 (split-clauses (cddr x)))))

(defun SPLIT-CLAUSES (list)
    (loop for (test action . rest) on list step #'cddr
          collect (list test action)))


                    
  
           
           
                 
                        
                        
  
           
            
