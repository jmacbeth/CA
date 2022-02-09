(in-package :cdparser)

;;; control.lisp was CA1.LSP -- contains most of the control structure for revised version of CA
;;; (rewritten in LISP by M. Burstein 11/7/79 from CA1.MLI)

;;; CA is the top-level function

;;; this now in globals.lisp
(declaim (special ALL-REQS ALL-CONS ALL-POOLS ALL-LEXES
                  WORD  C-LIST REQUEST-POOLS ; NEXT-WORD
                  EXTRA-REQUESTS FLAGS N-P-RECORDS LAST-EMBEDDED-CON CHANGED-CONS
                  ;; added
                  SENTENCE ; SENT
                  LEXICAL-POOL
                  *BINDINGS* ))

(defun CA (&optional in) ;; MB: added input arg to parse single sentence (more if list of lists)
  ;; :changed-cons holds concepts changed or added to C-LIST during one pass of CA
  (setq CHANGED-CONS nil)
  (setq CURRENT-PHRASE nil)
  (setq CURRENT-MODULE :CA)
  ;; MB added this to allow CA as call to parse a sentence(s)
  (setf INPUT (cond ((and (null in) input) input)
                    ((not (consp in)) (return-from ca nil))
                    ((not (consp (car in))) (list in)) ;; make it list of sentences
                    ((consp (car in)) in)))
  (pmsg :CA1 "Current Input:" input)
  ;; If the current sentence is finished, then get a new one and re-initialize;
  ;; (cond ((NULL SENTENCE) (init-ca)))
  ;; 
  (init-ca)
  (cond ((NULL SENTENCE) (setq WORKING nil) (return-from ca nil)))
  ;; (pmsg :CA1OPT T "Entering CA:" T "   C-LIST  = " (delinfree))
  ;; Set WORD (look it up in the dictionary);
  (loop for WORD = (get-next-item) ;; sets globals WORD, LEX
        while WORD
        do (format T "~2%======================= Current word: ~a ========================== ~%  Phrase: ~a rest: ~a" 
                   WORD CURRENT-PHRASE SENTENCE)

           ;; remove request pools which no longer contain active requests
           (clean-up-request-pools)
           (consider-lexical-requests) ;; considers all if no lexical requests
           (check-end-np) ;; moved COND below
           ;; activate requests associated with :word;
           (activate-item-requests WORD) ;; was LEX
           (consider-requests)
           (check-begin-np) ;; moved COND below

           ;; If any add-Con has built a CD which has a lot of memory stuff hooked onto
           ;; it, or if the next word might mark the end of the clause, then return to
           ;; let memory do its thing;

        when (or (flagon :MEMORY-HOOK-FLAG)
                 (CLAUSE-POINT) ;; took NEXT-WORD as arg before (see request-fns)
                 (flagon :RESULTS-FLAG))
          do 
             (pmsg :CA T "CA exiting. flags=" *flags*)
             (remove-flag :MEMORY-HOOK-FLAG)
             (remove-flag :RESULTS-FLAG)
             (pmsg :CATOP T "CA processed words:" CURRENT-PHRASE
                   T "  C-LIST =" (delinfree)
                   T)
             ;; (setq CHANGED-CONS (repl-unset-cons CHANGED-CONS)) ;;; COULDNT FIND THIS FN
             (setq CURRENT-MODULE  nil)
             (return T)
        ) ;; end loop     ;;;; no goto's! (T (GO CYCLE:))
  (pmsg :ca t "Result:" (xcons) t "C-LIST:"(delinfree) t)
  ;; added this as return (MB)
  ;; return the top level cons that weren't embedded during parse
  (remove-if #'(lambda (c) (get c :embedded)) (reverse C-LIST))   
  )


(defun check-end-np ()
  ;;If next work ends noun group, then return to regular node immediately,
  ;; so that next call to consider-requests (below) will consider all requests
  (cond ((and (flagon :noun-group-flag) (end-noun-phrase))
         (remove-flag :noun-group-flag)
         ;; (add-flag :results-flag) ;; this was causing termination after initial NP
         (pmsg :CA1 T "End of noun group;")))
  )

(defun check-begin-np ()
  ;; If next word begins a noun group, then begin noun-group mode immediately
  ;; so that the next call to consider-requests (above, but later in the loop)
  ;; will consider only latest requests;
  (cond ((and (not (flagon :noun-group-flag))
              (setq N-P-RECORD (begin-noun-phrase (next-word)))) ;; true only if an appropriate word next
         (add-flag :noun-group-flag)
         (if CHANGED-CONS (pmsg :CA1 T "Begin noun group;")))))


;;; This sets WORD, NEXT-WORD and SENT, as well as looking up NEXT-WORD.
;;; The first thing is to check if * flag was set, of so change the pause flag.

(defun GET-NEXT-ITEM ()
  (cond ((flagon :change-trace-flag)
         (setf *PAUSE* (not *PAUSE*))
         (remove-flag :change-trace-flag))
        )
  (setf WORD (pop SENTENCE))
  ;; (and WORD (SET WORD WORD)) ;; really???  This is probably just dangerous laziness (should take it out)
  ;; (setf SENTENCE (cdr SENTENCE))
  (cond ((equal (NEXT-WORD) '*)
         (add-flag :change-trace-flag)
         (setf SENTENCE (cdr SENTENCE))
         ;; (setf NEXT-WORD (car SENTENCE))
         ))
  ;; this was removed earlier was advancing NEXT-LEX to always be on next word
  ;; ;;  (setf LEX NEXT-LEX) 
  ;; Now see if we can Eliminate LEXes entirely and just generate pools directly on current word (MB)
  ;; (setf LEX (get-lex-info WORD)) ;; was NEXT-WORD for some kind of look-ahead?
  (push-end CURRENT-PHRASE  WORD)
  WORD)
         


;;; GET-LEX-INFO builds a lexical entry and sticks a request pool in it
;;; this fn from CA2 (only one used)
(defun GET-LEX-INFO (word) ;; removed first arg dest (not used)  ;; added word arg
  (let ((newlex (new-lex word)))
    #+ignore ;; we are not doing dynamic lookup
    (cond ((null (get word :requests))
           (look-up word)
           ))
    ;; replaced with next line (putprop newlex (build-pool (get word :requests)) :pool)
    (setf (get newlex :word) word) ;; MB - added this so I can follow it
    ;; put the gensym'd requests into :REQS prop on the lex (use to be a pool obj, but don't need the extra junk I think)
    (setf (get newlex :reqs)            ; :pool) (build-pool word
          (MAKE-REQUESTS word (get word :requests)))
  newlex
    )) 


;;; Activates requests associated with the current word, if any.
;;; EXTRA-REQUESTS holds requests built by requests in LEXICAL-POOL
;;; for activation in the next request pool built, which is here:

(defun ACTIVATE-ITEM-REQUESTS (wd) ;; was lex
  (let* (; (pool (get LEX :pool))
         ; (wd (get lex :word))
         ;; since we no longer call get-lex-info,
         ;; just make requests here instead of in GET-LEX-INFO ;; (and lex (get lex :reqs)))) 
         (reqs 
           (make-requests wd (get wd :requests))))
    (format t "~%ACTIVATE-ITEM-REQUESTS for word ~a: ~s" wd reqs)
    (cond ((or EXTRA-REQUESTS reqs)
           (activate-pool
            (build-pool wd
                        (append EXTRA-REQUESTS
                                (cond ((flagon :skip-word-flag)
                                       (remove-flag :skip-word-flag)
                                       nil)
                                      ; ((consp (car reqs)) (make-requests wd reqs))
                                      (t reqs)))))
           (setf EXTRA-REQUESTS nil)
           ))))



(defun NEXT-SENTENCE () (pop INPUT)) ;; first mention of var INPUT?

;;; Initialize the program
(defun INIT-CA ()
  (init-ca-vars)
  (setf SENTENCE (next-sentence))
;;  (setf SENT SENTENCE)
  (pmsg :initca T "New sentence is" T SENTENCE T)
  (setf LEXICAL-POOL nil) ; holds requests looking for specific words
  ;; initialize NEXT-WORD
;;  (setf NEXT-WORD (car SENTENCE))
  (cond ((equal (NEXT-WORD) '*)
         (add-flag :change-trace-flag)
         (setf SENTENCE (cdr SENTENCE))
         ;; (setf NEXT-WORD (car SENTENCE))
         ))
  ;; (get-lex-info (NEXT-WORD)) ;; first arg was '!next-lex -- removed. second arg was :NEXT-WORD (global)
  (begin-noun-phrase (next-word)) ;; here it is the first word. 
  )



;;; the code from CA3
;;; CA3.lisp contains the routines which test whether or not to switch the current mode. We are not happy
;;; with the way this is handled here. it's too syntactic and ad hoc to fit well with our analysis philosophy.
;;; This solution may be somewhat better than the original which came from Analole Gershman's NGP program.

;;; begin-noun-phrase assumes that the NOUN-GROUP-FLAG was NOT set. 
(defun BEGIN-NOUN-PHRASE (&optional (word (next-word)))
  (let ((np-req (find-pos-req word ; (NEXT-LEX)
                              '(adj arg name noun num poss titlel)))) ;; NEXT-LEX was !next-lex ??
    (when (setf N-P-RECORD np-req)
      (add-flag :noun-group-flag)
      (pmsg :CA1 T "Begin noun group:"))
    
    ;; (cond (np-req (put-first np-req (get (NEXT-LEX) :pool))))
    np-req
    ))

;;; PUT-FIRST takes an unactivated pool and a request and puts the request at the
;;; beginning of the pool. If the request already exists, it is removed from its previous position.

(defun PUT-FIRST (req pool)
  (setf (pool-reqs pool) (cons req (remove req (pool-reqs pool)))))


;;; find-pos-req looks for a request which has a pos entry which is in the list poslist.
;;; This is an example of a search through a list of requests; there will be more of these.

;;: MB: Trying to eliminate the layer of LEXes. Seems to be redundant with the reqs on words. 
;;; lex is now just the word, which has props :requests and :atts
;;; it looks like in some dictionaries, each request could have a POS att, but our dictionary doesnt have that
;;; at the moment. We short circuit this by changing this fn to look for pos in :atts

(defun FIND-POS-REQ (lex poslist)
  (let ((atts (get lex :atts))) ;; lex is now just a word
;;; I think the old SOME return'd all matches - replace with REMOVE-IF-NOT to have that effect
    (remove-if-not #'(lambda (x) (member x poslist)) atts)))
  #+ignore
  (car (some #'(lambda (x) (member (get-pos x) poslist))
             (pool-reqs (get lex :pool))))

;;; get-pos gets the part-of-speech entry out of a request

(defun GET-POS (req)
  (second (assoc 'pos req)))
  #+ignore
  (let ((pos-pos (some #'(lambda (x) (eq (car x) 'pos)) req)))
    (cond (pos-pos (second pos-pos))))

;;; end-noun-phrase assumes that the NOUN-GROUP-FLAG is set (see CA3 for original version)
(defun END-NOUN-PHRASE ()
  (let ((atts (begin-noun-phrase (next-word))))
    (cond ((and atts
                (or (and (member 'ART atts) (not N-P-RECORD))
                    (and (intersection ATTS '(ADJ NUM))
                         (not (intersection N-P-RECORD '(noun title name))))
                    (and (intersection ATTS '(title noun))
                         (not (member 'name N-P-RECORD)))
                    (and (member 'name ATTS)
                         (not (member 'NOUN N-P-RECORD)))
                    ))
            ;; not end-n-p, so update n-p-record
           (setf N-P-RECORD (union ATTS N-P-RECORD))
           nil)
          (t ;; all else
           ;; then (go :end-n-p) - gathered here
           (setf N-P-RECORD nil)
           (remove-flag :noun-group-flag)
           t) ;; return T if ending np
          )))



         


;;; Removes pools which no longer contain active requests from REQUEST-POOLS
(defun CLEAN-UP-REQUEST-POOLS ()
  (clean-up-special-pools)
  (setf REQUEST-POOLS
        (loop for p in request-pools
              when (live-reqs p)
              collect p)))


;;; pool still contains some live requests
(defun LIVE-REQS (pool)
  (some #'(lambda (r) (get r :active))
        (pool-reqs pool)))

;;; returns list of live requests in pool
(defun SAVE-LIVE-REQS (pool)
  (setf (pool-reqs pool)
        (remove-if-not #'(lambda (r) (get r :active))
                       (pool-reqs pool))))

(defun CLEAN-UP-SPECIAL-POOLS ()
  (save-live-reqs 'LEXICAL-POOL))

;;; This routine considers the requests with tests which are looking for specific words,
;;; which are contained in LEXICAL-POOL
(defun CONSIDER-LEXICAL-REQUESTS ()
  (cond ((pool-reqs 'LEXICAL-POOL)
         (pmsg :ca1 t "Considering lexical requests:")
         (consider-pool 'LEXICAL-POOL))
        (t ; (pmsg :CA1 T "No lexical reqs. Moving to consider all requests.")
           (consider-all-requests))))


;;; considers requests depending on mode setting
(defun consider-requests ()
  (cond ((flagon :noun-group-flag)
         ; (pmsg :ca1 T "Considering latest requests:")
         (consider-latest-requests))
        (t 
           (consider-all-requests))))


;; This eventually needs to be more sophisticated, handling the problem of request priority
;;; within a given pool.
;;; It returns nil unless at least one of the calls to consider returned T.

(defun CONSIDER-POOL (pool)
  (let ((reqs (pool-reqs pool)))
    (when reqs
      ; (pmsg :CA1 T "Considering pool" pool reqs)
      (mapcan #'(lambda (req) (and (consider req pool) (list t)))
              reqs))))

#| This is a bit changed from the original description. In order to be somewhat more "depth-first",
whenever a request is triggered, instead of continuing down the list of request pools, 
the program immediately goes back to the most receent ones again. 
This is especially important if any new requests were spawned. Note that at 
present, consider-pool does not operate that way, so all the requests in a pool
get considered even if one has already triggered. 
(obviously, REQUEST-POOLS no longer REVERSEd every time through. - kept as stack (mb)
|#
;; was DEX
(defun CONSIDER-ALL-REQUESTS ()
  (when request-pools
    ; (pmsg :CA1 T "CONSIDER-ALL-REQUESTS: pools=" request-pools)
    (loop while (some #'consider-pool REQUEST-POOLS))))

;;; This only considers requests in the most recent pool,
;;; including any requests spawned by them (comments above apply here)
(defun CONSIDER-LATEST-REQUESTS () ;; was DEX
  (let (; (latest-pool (car REQUEST-POOLS))
        (older-pools (cdr request-pools)))
    ;; was (member LATEST-POOL REQUEST-POOLS) but assume this is a stack
    ;; list of pools from latest-pool to front of list
    (loop for latest-pools = (ldiff REQUEST-POOLS (cdr older-pools)) 
          ;; do (pmsg :ca1 t "CONSIDER-LATEST-REQUESTS latest pools:" latest-pools)
          while (some #'consider-pool latest-pools))))
                      


;;; MB added this to create real local bindings that can be passed to eval-action
;;; collect all the assignment vars from := forms in a request

(defun collect-vars1 (form &optional (bindings nil))
  (cond ((not (consp form)) nil)
        ((eq (car form) :=)
         (or (assoc (second form) bindings)
             (push (cons (second form) nil) bindings)))
        (t (setf bindings (merge-blists
                           (collect-vars1 (car form) bindings)
                           (collect-vars1 (cdr form) bindings)))))
  bindings)

(defun merge-blists (a b)
  (remove-duplicates (append a b) :test #'equal))


(defun collect-vars (form &optional (bindings nil))
  ; (when (eq (caar form) 'activate-next) (break))
  (let ((foundvars (collect-vars1 form bindings)))
    ;;(format t "~%Collect-vars found ~a from bdgs ~a" foundvars bindings)
    (values foundvars (mapcar #'(lambda (s) (list (car s) `',(cdr s))) foundvars))))
        

(defun bind (var val)
  (declare (special *bindings*))
  (let ((cell (or (assoc var *bindings*)(car (push (cons var nil) *bindings*)))))
    (setf (cdr cell) val)
    ; (format t "~%in BIND *bs*= ~s" *bindings*)
    val))

(defun bindings-as-letvars (bndgs)
  (mapcar #'(lambda (b) `(,(car b) ',(cdr b))) bndgs))

(defmacro := (var $val)  ;; was a DFX!
  `(setf ,var (bind ',var ,$val)) ;; for passing to actions
  )


;;; MB rewritten to potentially be more like compilable. if test and actions compiled as a when,
;; then wouldnt need binding list, just let vars around whole body. 


;;; Evaluate the text of a request clause
;;; User of := in test is done by a fexpr
(defun EVAL-TEST (req cl bindings)
  (multiple-value-bind (bdgs vars) (collect-vars cl bindings)
    (let* ((tstform (cdr (assoc 'test cl)))
           (form 
             `(let ((*bindings* ',bdgs)
                    (current-req ',req)
                    (res nil)
                    ,@vars
                    )
                ,@(butlast tstform)
                (setf res ,(car (last tstform)))
                (values res *bindings*))))
      ;; (pmsg :ca t 'eval-test vars t :form form)
      (multiple-value-bind (res bds) (eval form)
        ;; (format t "~%EVAL-TEST ~a result: ~a  bdgs: ~a" req res bds)
        (values res bds))
      )))

;;; Evaluate the actions in a request clause
;;; Note that if a pseudo-assignment appears in an action, eval-action itself does the work - the function ::= is not called

;; cl like ((test t) (actions ...))
(defun EVAL-ACTIONS (req cl bindings &optional pool)
  (multiple-value-bind (bdgs vars) (collect-vars cl bindings)
    (let* ((act-list (cdr (assoc 'actions cl)))
           (bvars (bindings-as-letvars bindings))
           (act-vars (collect-vars act-list bindings))
           (eform `(let (,@vars
                         (*bindings* ',bdgs)
                         (current-req ',req)
                         (current-pool ',pool)
                         )
                     ,@act-list)))
      ;; (format t "~%EVAL-ACTIONS ~a effective bindings ~s" req bdgs)
    ;; (pmsg :CA1 t "Action list is" act-list  t "Test Bindings:"  bvars "Act-vars: " act-vars)
      (eval eform))))


#+ignore
  (loop with action = nil
        for act-list =  ;; was cdadr 
          then (cdr act-list)
        while act-list
        do 

           (setf action (car act-list))
           (cond ((eq (car action) ':=)
                  (setf act-list (subst (list 'quote (eval (third action))) (second action) act-list))
                  )
                 (t (eval action))))


;;; checks each test-action pair of a request in turn.
;;; if some test true, then associated action is eval'd


(defun CONSIDER (request pool)
  (let ((NEW-CON nil) ;; was called !NEW-CON for some reason
        (body (get request :body))
        (tracep (or (get request :trace) (get request 'trace)))
        (bindings (get request :bindings)) ;; added mb
        ) 
    (cond ((get request :active)
           (when tracep
               (pmsg :ca1 t "CONSIDERing active request" request) ;;  " body:  " body)
               (pmsg :ca1 t "   bindings:" bindings)
               )
           (some #'(lambda (clause)
                     (multiple-value-bind (res tstbindings) (eval-test request clause bindings)
                       (cond (res
                              (pmsg :CA T request "has fired" );  . body=" body t "bindings: " bindings
                                        ;  T "C-LIST - " (delinfree) t)
                              ;; (format t "~%after test bindings: ~a" tstbindings)
                              (putprop request nil :active)
                              (eval-actions request clause tstbindings pool)))))
                 body)


           ;; if request fired, (active prop is nil) then return T
           ;; also, if NO-KILL-FLAG set then reactivate the request
           (cond ((null (get request :active))
                  (cond ((flagon :no-kill-flag)
                         (remove-flag :no-kill-flag)
                         (putprop request t :active)))
                  t)
                 (t nil))
           ))))


;;; When a request knows what to do with the next word (by checking get-next-item)
;;; then don't load the definitions under the new word.
;;; (see activate-item-requests)

;;; MB: This will have to be redone since we no longer use get-next-item to preload
;;; the next word's requests.  Not needed thus far. 

(defun SKIP-NEXT-WORD () (add-flag :skip-word-flag))

;;; BUILD-POOL takes a dictionary and builds it into an active structure.
;; That means constructing a request pool out of it.

(defun BUILD-POOL (wd new-requests)
  (let ((pool (new-pool wd)))
    ; (putprop pool new-requests :specs)
    (setf (pool-reqs pool) new-requests) ;; (make-requests wd new-requests) ;; so now does a (set pool new-reqs)
    pool))

;;; ACTIVATE-POOL takes a pool which has been built using BUILD-POOL and puts it on the list of request-pools
(defun ACTIVATE-POOL (pool)
  (pushnew pool REQUEST-POOLS) ;; was push
  request-pools)

;;; Given a list of requests in dictionary format, this puts them into active format
(defun MAKE-REQUESTS (wd &optional (reqs (get wd :requests)) bindings)
  ;; (format t "~%In MAKE-REQUESTS bindings: ~a" bindings)
  (loop for req in reqs
        collecting (gen-request req wd bindings)))

;; changes one request from dictionary format to active format
(defun GEN-REQUEST (R wd &optional (bindings nil))
  (let ((reqsym (new-req wd))
        body)
    (if (symbolp R)
        (setf R (symbol-value R))) ;; for named requests
    (if (eq (car R) 'request) (setf R (cdr R)))
    (setf (get reqsym :body) (clausify R)) ;; remove 'clause at head of R first
    (setf (get reqsym :word) wd) ;; mb added
    (setf (get reqsym :bindings) bindings) ;; mb added - used by CONSIDER in lieu of using closures
    ;;; carry through *req-props* (see dictionary-fns)
    (loop for form in R
          unless (eq (car form) 'clause)
            ;; add properties that were not clauses
            do (setf (get reqsym (car form)) (cdr form)))
    (setf (get reqsym :active) T)
    ; (format t "~%New request ~a has body ~a" reqsym (get reqsym :body))
    reqsym))

;;; clausify filters out non-request-related information from a lexical specification

(defun CLAUSIFY (r)
  (let ((clauses (if (eq (car R) 'request) (cdr R) R))) ;; remove 'request
    (loop for clause in clauses
          when (eq (car clause) 'clause)
          collect 
          (list (assoc 'test (cdr clause))
                (assoc 'actions (cdr clause))))))
  ; (split-clauses (loop for y in x when (member (car y) '(test action)) collect y)))

;; changes a list of test-action-pairs from dictionary format to internal clause format. (T A T A... ) --> ((T A) (T A)..)
;;;; this was recursive:
#+ignore
(defun split-clauses (x)
  (and x (append (list (list (car x) (cadr x)))
                 (split-clauses (cddr x)))))




                    
  
           
           
                 
                        
                        
  
           
            
