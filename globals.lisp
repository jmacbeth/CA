(in-package :cdparser)



(defparameter CHANGED-CONS nil)
(defparameter CURRENT-PHRASE nil)
(defparameter CURRENT-MODULE :CA)
(defparameter WORKING nil)
(defparameter N-P-RECORD nil)
; (defparameter NEXT-WORD nil)
(defparameter *PAUSE* nil)

;;; declared in CA4 (request-actions.lisp)
; (defparameter NEW-CON nil) ;; (was called !NEW-CON) 
(defparameter LEXICAL-POOL nil)
(defparameter LAST-EMBEDDED-CON nil)
(defparameter CHANGED-CONS nil)
(defparameter *BINDINGS* nil) ;; just to declare it special. It will be scoped. 

;;; from control.lisp mostly
;;;; THIS SHOULD BE REORG'd into a single top level data structure, not all these globals

(defparameter ALL-REQS nil)
(defparameter ALL-CONS nil)
(defparameter ALL-POOLS nil)
(defparameter CURRENT-POOL nil) ;; set when considering requests in that pool (MB 2/1/21)
(defparameter CURRENT-REQ nil)
(defparameter ALL-LEXES nil)
; (defparameter SENT nil)  ;; redundant with SENTENCE - changed all occurrences to SENTENCE
(defparameter SENTENCE nil)
(defparameter INPUT nil) ;; list of sentence s-expressions, pop'd into SENTENCE one at a time


(defparameter WORD nil) ;; the current word after pop'd off sentence
(defparameter *defined-words* nil) ;; added by MB to keep track of defterms
;;; changing this to a macro
;; (defparameter NEXT-WORD nil)
(defmacro NEXT-WORD () `(car SENTENCE))


(defparameter C-LIST nil)
(defparameter REQUEST-POOLS nil)
(defparameter EXTRA-REQUESTS nil)
(defparameter N-P-RECORDS nil)
(defparameter LAST-EMBEDDED-CON nil)
(defparameter CHANGED-CONS nil)
(defparameter LEX nil)   ;; 
(defparameter NEXT-LEX nil)
(defparameter POOL nil)

;;; all flags are placed on the list :flags; the following functions test and remove flags
(defparameter *flags* nil)

(defmacro flagon (flag) `(member ,flag *flags*))

(defmacro remove-flag (flag)
  `(setf *flags* (remove ,flag *flags*)))

(defmacro add-flag (flag)
  `(pushnew ,flag *flags*))


;;; This initializes some variables - book-keeping globals containing generated gensyms
(defun INIT-CA-VARS ()
  (setf ALL-REQS nil)
  (setf ALL-CONS nil)
  (setf ALL-POOLS nil)
  (setf ALL-LEXES nil)
  (setf WORD nil)
  ;; (setf NEXT-WORD nil) ;; now use macro
  (setf C-LIST nil)
  (setf REQUEST-POOLS nil)
  (setf EXTRA-REQUESTS nil)
  (setf *FLAGS* nil)
  (setf N-P-RECORDS nil)
  (setf LAST-EMBEDDED-CON nil)
  (setf CHANGED-CONS nil)
  ; (setf SENT nil)
  (setf SENTENCE nil))


