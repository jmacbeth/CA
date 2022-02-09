(in-package :cdparser)

;;; This was the file CRASH.DIC. It contains CA definitions for the words in the first DSSAM story.
;;; The predicates used in the tests in particular need to be defined.

;;; MB: I have changed the syntax to use keywords for the parts
;;; and dropped the head atoms REQUEST and CLAUSE, replacing
;;;          (DEF <word> (ATTS: <pos>*) (REQUESTS: (REQUEST (TEST <test>)(ACTIONS <action>*))*))
;;;     with (DEFTERM <word> (<atts>) <request>*)
;;; 


;;; where it was (REQUESTS: (REQUEST (CLAUSE (test -test-)(ACTIONS -actions-))* )
;;; it is now like a lisp COND. so  <req> = (REQ (<test> <action>*)*)
;;; and  (ACTIVATE (REQ ((<test><action>*))*))
;;;  --> (activate (request (TEST <test>)(ACTIONS <action>*))*)
;;; is expanded by a macro to be as it was in the original file

;;; simple example of old->new

#+ignore
(def A 
  (:atts art)
  (:requests
   (request
    (clause (test t)
            (actions
             (:= str1 (build-con '(*indef*) nil nil))
             (activate
              (request
               (clause (test (:= str2 (if-find #'(lambda (foo) (or (feature foo 'loc) (feature foo 'pp))))))
                       (actions (fill-gap (add-gap '(ref) str2 str2 str1)))))))))))

;;; also IF-FIND function was changed to not require a full lambda, instead binding the variable C
;;; to concept from the C-LIST it is testing in the s-expression test that is its argument which
;;; was previously the lambda BODY. 

;;; := binds the variable (its first arg) to the result of eval-ing the second arg form in
;;; the request test context, which also is being tested by the CONSIDER function.
;;; CONSIDER is called by CONSIDER-POOL, which is a set of requests (agenda rules) to be
;;; tested after activating the requests associated with the current WORD. 



(defterm A (ART)
  (REQ (t  ;; this is the test
        ;; these are the actions
        (:= str1 (build-con (*indef*))) ;; make extra args optional (nil nil)
        (ACTIVATE (REQ ; (:trace t)
                                      ;; wait for noun group to form before attaching :ref (*indef*)
                       ((:= str2 (and (not (flagon :noun-group-flag))
                                      (if-find (feature c '(loc pp)))))
                         (format t "~%A = ~a found pp ~a = ~a" str1 str2 str1)
                        (fill-gap :ref str2 str1)
                        ))))))

(defterm THE (ART)
  (REQ (t
        (:= str1 (build-con (*def*)))
                                      ;; wait for noun group to form before attaching :ref (*def*)
        (ACTIVATE (REQ ((:= str2 (and (not (flagon :noun-group-flag))
                                      (if-find (feature c '(loc pp)))))
                        (fill-gap :ref str2 str1)
                        ))))))

(defterm SMALL (ADJ)
  (REQ (t
        (:= str1 (build-con (*ltnorm*)))
                                      ;; could wait for full noun group concept to form before 'small'
        (ACTIVATE (REQ ((:= str2 (if-find (feature c 'pp)))
                        (fill-gap :size str2 str1)
                        ))))))

(defterm TWIN-ENGINE (ADJ)
  (REQ (t
        (:= str1 (build-con (*PP* :class (GROUP) :number (*num* number (*2*))
                                  :member (*PP* :class (structure) :type (*engine*)))))
        (ACTIVATE (REQ ((:= STR2 (if-find (and (feature c 'pp) (follows c str1))))
                        (fill-gap :has-part str2 str1)))))))

(defterm PLANE (NOUN)
  (REQ (t (BUILD-CON (*pp* :class (VEHICLE) :type (*AIRPLANE*))))))

(defterm PILOT (NOUN)
  (REQ (t (BUILD-CON (*pp* :class (HUMAN) :type (*PILOT*))))))

(defterm STUFFED (VERB)
  (REQ (t (:= STR1 (build-con (*do* <=> (*ptrans*) :actor (nil) :object (nil)
                                :to (*inside* :part (nil)) :from (nil) :time (nil))))
          (ACTIVATE-LEXICAL-REQS WITH ;; MB- added the word
           (REQ ((eq WORD 'with) ;; this is how it handled many prepositions!!
                 (ACTIVATE-NEXT ;; was activate-in-next-pool
                  (REQ ((:= str2
                            (if-find (and (feature c 'pp)
                                          (not (feature c 'hi-anim)))))
                        (fill-gap :rel str2 str1)
                        (subst-cd str2 (get-role-filler '(:to :part) str1) str1))
                       (t (kill-self))))))
           (REQ ((eq WORD 'with)
                 (activate-next ;; was activate-in-next-pool
                  (REQ ((and (is-filled '(:to :part) str1)
                             (:= str3 (if-find (feature c 'PHYSOBJ))))
                       (fill-gap :object str1 str3))))))
          ))))

(defterm *1500* (NUM)
  (REQ (t (build-con (*num* number (*1500*))))))

(defterm *10* (NUM)
  (REQ (t (build-con (*num* number (*10*))))))

(defterm POUNDS (noun)
  (REQ (t (:== str1 (build-con (*pp* :class (unit) :type (*lb*) :number (nil))))
          (ACTIVATE
           (REQ ((:= str2 (if-find (feature C 'number)))
                 (fill-gap :number str1 str2)))))
          (ACTIVATE-LEXICAL-REQS OF
           (REQ ((eq WORD 'OF)
                 (ACTIVATE-NEXT ;; was activate-in-next-pool
                  (REQ ((:= str3 (if-find (and (feature C 'pp) (follows C str1))))
                        (fill-gap :amount str3 str1)))))))))

(defterm MARIJUANA (NOUN)
  (REQ (t (BUILD-CON (*pp* :class (PHYSOBJ) :type (*MJ*))))))

(defterm CRASHED (VERB)
  (REQ (t (:= STR1 (build-con (*do* <=> ($CRASH) :object (nil) :place (nil))))
          (ACTIVATE
           (REQ ((:= str2 (if-find (feature C 'VEHICLE)))
                 (fill-gap ':object str1 str2)))
           (REQ ((:= str3 (if-find (feature c 'loc)))
                 (fill-gap :place str1 str3))))
           )))

(defterm MILES (noun)
  (REQ (t (:= str1 (build-con (*pp* :class (unit) :type (*mile*) :number (nil))))
          (ACTIVATE
           (REQ ((:= str2 (if-find '(feature C 'number)))
                 (fill-gap :number str1 str2)))))))

(defterm SOUTH (noun)
  (REQ (t (:= str1 (build-con (*south*)))
          (ACTIVATE
           (REQ ((:= str2 (if-find (feature c 'unit)))
                 (:= str3 (build-con (*loc* :prox (nil) :dir (nil) :dist (nil))))
                 (fill-gap ':dir str3 str1)
                 (fill-gap ':dist str3 str2)
                 (ACTIVATE-LEXICAL-REQS OF
                  (REQ ((eq WORD 'of)
                       (activate-next ;; was activate-in-next-pool
                        (REQ ((:= str4 (if-find (and (feature c 'loc)
                                                     (not (eq c str3)))))
                              (fill-gap :prox str3 str4))))))))))
          (ACTIVATE-LEXICAL-REQS OF
            (REQ ((eq WORD 'of)
                  (:= str3 (build-con (*loc* :prox (nil) :dir (nil))))
                  (ACTIVATE-NEXT ;; was activate-in-next-pool
                                 (REQ ((:= str4 (if-find (and (feature c 'loc)
                                                              (not (eq c str3)))))
                                       (fill-gap :dir str3 str1)
                                       (fill-gap :prox str3 str4)))))))
          )))

(defterm HERE (noun)
  (REQ (t (build-con (*loc* :prox (*here*))))))

(defterm YESTERDAY (noun)
  (REQ (t (:= str1 (build-con (*yesterday*)))
          (ACTIVATE
           (REQ ((:= str2 (if-find (feature C 'concept)))
                 (fill-gap :time str2 str1)))))))

(defterm AS (conj)
  (REQ (t (:= str1 (build-con (*conrel* :type (*when*) :cona (nil) :conb (nil))))
          (ACTIVATE
           (REQ ((:= str2 (if-find (and (feature c 'concept)
                                         (not (eq c str1)))))
                 (fill-gap :cona str1 str2)))
           (REQ ((:= str3 (if-find (and (feature c 'concept)
                                         (not (eq c str1)))))
                 (fill-gap :conb str1 str3)))))))

(defterm IT (pron)
  (REQ (t (build-con (*pp* :class (nil) :ref (*def*))))))


;;; mb - I changed slots VEH to :actor and FIELD to :to
;;; treating approach as a script is bogus anyway
(defterm APPROACHED (verb) 
  (REQ (t (:= str1 (build-con (*do* <=> ($approach) :actor (nil) :to (nil))))
          ;;; this all could be better (gist of original comment)
          (ACTIVATE
           (REQ ((:= str2 (if-find (feature c 'pp)))
                 (fill-gap :actor str1 str2)))
           (REQ ((:= str2 (if-find (feature c 'loc)))
                 (fill-gap :to str1 str3)))))))

;;; bogus anyway... 
(defterm LANDING (noun)
  (REQ ((eq (next-word) 'strip) ;; shouldnt it be next word? ;; (next-word) was (car SENTENCE)
                                   ; what was this doing here??? (setf SENT (cdr SENT))
        (build-con (*loc* :type (*landing-field*))))))
                
(defterm KILLING (verb)
  (REQ (t (:= str1 (build-con (*leadto* :actor (nil) ;; changed from *conrel* :type (*leadto*)
                                :ante (nil) :conse (*be* :state (*health* :val (-10))))))
          (ACTIVATE
           (req ((:= str2 (if-find (feature c 'anim)))
                 (fill-gap '(:conse :actor) str1 str2)))))))


                     
                                         
       
        
                


           
                              
          
