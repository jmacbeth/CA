(in-package :cdparser)

;;; This was the original function is used in writing dictionary entries - the usual form is
;;; (def word
;;;      (atts: adj noun ..)
;;;      (requests: (request ...)*)
;;; for commonlisp, we should replace atts: with :atts, requests: with :requests


;; this just sets a bunch of properties on the word symbol
;;; if there are other attributes than ATTS and REQUESTS, I don't have examples in the code that was rescued.

(defmacro def (word . prop-vals)
  `(mapc #'(lambda (k-v)
               (setf (get ,word (car k-v)) (cdr k-v)))
         ,prop-vals))



;;; Instead I tried to simplify things a bit to so I wouldnt have to do as much typing
#| ;for example
(defterm THE (ART)
  (REQ (t
        (:= str1 (build-con (*def*))) ;; make extra args optional (nil nil)
        (ACTIVATE (REQ ((:= str2 ; (if-find #'(lambda (foo) (or (feature foo 'loc) (feature foo 'pp)))))
                          (find-con (has-feature c '(loc pp))))
                        ;; (fill-gap (add-gap '(ref) str2) str2 str1)
                        (fill-gap :ref str2 str1)
                        ))))))

rather than 
(def A 
  (:atts art)
  (:requests
   (request
    (clause (test t)
            (actions
             (:= str1 (build-con '(*indef*) nil nil))
             (activate
              (request
               (clause (test (:= str2 (if-find #'(lambda (foo) 
                                                   (or (feature foo 'loc) 
                                                       (feature foo 'pp))))))
                       (actions (fill-gap (add-gap '(ref) str2)
                                          str2 str1))))))))))


|#

;;; for now just try to make it look like it is putting the right properties on word.

;;; support for tracing (MB)
;;; there may be other properties we want to put on request definitions...
(defparameter *req-props* '(trace :trace))

;;; a reqform is (REQ (<test> <action>*)*) --> (REQUEST (CLAUSE (TEST <test>)(ACTIONS <action>*))*)
(defun expand-req (reqform)
  (when (eq (car reqform) 'REQ)
    (cons 'REQUEST 
          (loop for c in (cdr reqform)
                when (member (car c) *req-props*)
                  collect c into props
                else collect c into clauses
                finally
                   (return 
                     (append props
                             (loop for clause in clauses
                                   for acts = (expand-sub-reqs (cdr clause))
                                   collect `(CLAUSE (TEST ,(car clause))
                                                    (ACTIONS ,@acts)))
                      ))))))

(defun expand-sub-reqs (acts &aux newacts newsub)
  (loop with newacts = acts
        for sub-req = (sub-head-p 'req newacts)
        while sub-req
        do (setf newsub (expand-req sub-req))
           (setf newacts (subst newsub sub-req newacts))
        finally (return newacts)))


;;; these should just get loaded, not compiled for now. 
(defmacro defterm (word atts . reqs)
  (let ((requests
          (loop for reqform in reqs
                collect (expand-req reqform))))
  `(eval-when (:load-toplevel :execute :compile-toplevel)
     (prog ()
       (pushnew ',word *defined-words*)
       (setf (get ',word :atts) ',atts)
       (setf (get ',word :requests) ',requests)))))
           
  
