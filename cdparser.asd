(in-package :cl-user)

(defpackage :cdparser-asd (:use :asdf :common-lisp))
(in-package :cdparser-asd)

(defsystem :cdparser
  :depends-on ()
  :serial t
  :components (
               (:file "package")
               (:file "globals")
               (:file "macros")  ;; was basically CA0 
               (:file "utils")
               (:file "control") ;; was CA1
               ;; CA2 was dictionary lookup routines suitable only for UCI-LISP
               ;; CA3 contained a couple bracketing routines folded into control
               (:file "dictionary-fns")
               (:file "concept-fns") ;; was CA5 - build CD structures
               (:file "request-fns") ;; was CA4 and CA6
               (:file "predicates") ;; for testing concepts was CA7
               (:file "named-requests") ;; was CA8 - we may not need this for simple test.
               (:file "crash-dic") ;; definitions of words in test sentences
               (:file "test") ;; test sentence
  ))

(provide 'cdparser)
