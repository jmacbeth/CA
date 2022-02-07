;;; CA1.LSP -- contains most of the control structure for revised version of CA
;;; (rewritten in LISP by M. Burstein 11/7/79 from CA1.MLI)

;;; CA is the top-level function

(defparameter *changed-cons* nil)
(defparameter *current-phrase* nil)
(defparameter *current-module '@CA)
(defparameter *sent* nil)
(defparameter *working* nil)

(defparameter *pmsg-flags* T) ;; show all msgs if T, else a list of keywords

(defmacro pmsg (&rest args)
  `(when (and *pmsg-flags*
              (or (eq *pmsg-flags* t)
                  (member (first ,args) *pmsg-flags*)))
     (loop for arg in ,(cdr args)
           do (cond ((eq arg t) (terpri))
                    (t (princ arg))))))



(de CA nil
    ;; :changed-cons holds concepts changed or added to :C-list during one pass of CA
    (setq *changed-cons* nil)
    (setq *current-phrase* nil)
    (setq *current-module* :CA)
    ;; If the current sentence is finished, then get a new one and re-initialize;
    (cond ((NULL *sent*) (init-ca)))
    (cond ((NULL *sent*) (setq *WORKING* nil) (return-from ca nil)))
    (pmsg :CA1OPT T "Entering CA:" T "   :C-list  = " (delinfree))
    ;; Set :word and :next-word, and look them up in the dictionary;
   (loop for word = (get-next-item)
          while word
          ;;  CYCLE: (get-next-item) ;; NO OPEN CODED GOTO's!!!
          ;;  (cond ((NULL :word) (return nil)))
          do 
             (pmsg :CA T "Beginning main execution cycle;"
                   T ":word = " :word
                   T "*sent* = " *sent*
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
                         (setq :N-P-record (begin-noun-phrase)))
                    (add-flag :noun-group-flag)
                    (if *changed-cons* (pmsg :CA1 T "Begin noun group;"))))

             ;; If any add-Con has built a CD which has a lot of memory stuff hooked onto
             ;; it, or if the next word might mark the end of the clause, then return to
             ;; let memory do its thing;
             ;; (cond (
             when (or (flagon :MEMORY-HOOK-flag)
                        (CLAUSE-POINT :next-word)
                        (flagon :results-flag))
               do 
                  (remove-flag :MEMORY-HOOK-flag)
                  (remove-flag :results-flag)
                  (pmsg :CATOP T "CA processed words: " *current-phrase*
                          T "  :C-list = " (delinfree)
                          T)
                  (setq *changed-cons* (REPL-UNSET-cons *changed-cons*))
                  (setq *current-module*  nil)
                  (return T)
                  ) ;; end loop     ;;;; no goto's! (T (GO CYCLE:))
   ))
;; )


