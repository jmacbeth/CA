(in-package :cdparser)

;;; CA3.lisp contains the routines which test whether or not to switch the current mode. We are not happy
;;; with the way this is handled here. it's too syntactic and ad hoc to fit well with our analysis philosophy.
;;; This solution may be somewhat better than the original which came from Analole Gershman's NGP program.

;;; begin-noun-phrase assumes that the NOUN-GROUP-FLAG was NOT set. 
(defun begin-noun-phrase ()
  (let ((np-reg (find-pos-reg NEXT-LEX '(adj arg name noun num poss titlel)))) ;; NEXT-LEX was !next-lex ??
    (cond (np-reg (put-first np-reg (get NEXT-LEX :pool))))))

;;; find-pos-reg looks for a request which has a pos entry which is in the list poslist.
;;; This is an example of a search through a list of requests; there will be more of these.

(defun find-pos-reg (lex poslist)
  (car (some #'(lambda (x) (member (get-pos x) poslist))
             (get (get lex :pool) :specs))))

;;; get-pos gets the part-of-speech entry out of a request

(defun get-pos (reg)
  (let ((pos-pos (some #'lambda (x) (eq (car x) 'pos)) req))
    (cond (pos-pos (second pos-pos)))))

;;; end-noun-phrase assumes that the NOUN-GROUP-FLAG is set
(defun end-noun-phrase ()
  (let ((atts (begin-noun-phrase)))
    (cond ((and atts
                (or (and (member 'ART atts) (not N-P-RECORD))
                    (and (intersection ATTS '(ADJ NUM))
                         (not (intersection N-P-RECORD '(noun title name))))
                    (and (intersection ATTS '(title noun))
                         (not (member 'name N-P-RECORD)))
                    (and (member name ATTS)
                         (not (member 'NOUN N-P-RECORD)))
                    ))
            ;; not end-n-p, so update n-p-record
           (setf N-P-RECORD (union ATTS N-P-RECORD))
           nil)
          (t ;; all else
           ;; then (go :end-n-p) - gathered here
            (setf N-P-RECORD nil)
           t) ;; return T if ending np
          )))


;; I put the original form here in case I muffed it up:
#+ignore
(de end-noun-phrase ()
    (prog (atts)
         (setq atts (begin-noun-phrase))
       (cond ((null atts) (go :END-N-P))
             ((member 'art atts)
              (if N-P-RECORD (go :END-N-P) (go :NOT-END)))
             ((intersection ATTS '(adj num))
              (if (intersection N-P-RECORD '(noun title name)) (go :END-N-P) (go :NOT-END)))
             ((intersection ATTS '(title noun))
              (if (member 'name N-P-RECORD) (go :END-N-P) (go :NOT-END)))
             ((member 'name ATTS)
              (if (member 'nnoun N-P-RECORD) (go :END-N-P) (go :NOT-END)))
             (t (go :END-N-P)))
     :NOT-END ;; update n-p-record
       (setf N-P-PRECORD (UNION ATTS N-P-RECORD))
       (return nil)
     :END-N-P
       (setf N-P-RECORD NIL)
       (RETURN T)))

