(in-package :cdparser)
;;; this was CRASH.IDX. It appeared to just say that the words in this sentence
;;; were found in CRASH.DIC

;;; I don't know why 'of' was left out in several places, so I put them back
(defvar *crash-sentence*
  '(a small twin-engine plane stuffed with *1500* pounds of marijuana crashed *10* miles south of here yesterday
    as it approached landing killing the pilot))

(defparameter tst1 '(a small plane crashed yesterday))
(defparameter tst2 '(a small plane crashed here yesterday)) ;; works
(defparameter tst3 '(a small plane crashed south of here yesterday)) 
(defparameter tst4 '(a small twin-engine plane stuffed with marijuana crashed yesterday))
(defparameter tst5 '(a small twin-engine plane stuffed with marijuana crashed here yesterday))
(defparameter tst6 '(a small twin-engine plane stuffed with marijuana crashed south of here yesterday))

