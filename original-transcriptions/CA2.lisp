(in-package :cdparser)

;;; CA2.lsp dictionary lookup routines
;;; All except build-lex-info taken from Larry Birnbaum's CA2.mli
;;; which was originally cannibalized from ELI  (Riesbeck's parser)


(defparameter PPN nil)
(defparameter DEV :dsk) ;; need this?
(defparameter FILE nil)
(defparameter PREV nil)

;;; INCONTENTS checks the associatte content list, if any, for the entry

(defun incontents (x file)
  (let ((contents (get file :contents)))
    (or (null contents) (member x contents))))

;;; GET-LEX-INFO builds a lexical entry and sticks a request pool in it

(defun get-lex-info (dest word)
  (let ((new (new-lex)))
    (cond ((null (get word :requests))
           (look-up word)))
    ;(putprop new (build-pool (get word :requests)) :pool)
    (setf (get new :pool) (build-pool (get word :requests)))))

;;; THIS WILL NEED TO BE REWRITTEN TO USE A HASH TABLE instead of trying to reparse the dictionary files.
;;; and for different stream-based IO model

#||
;;; Note: INC is the Stanford/UCI-LISP function for input channel specification.  (see stanford AI lisp manual): 
(INPUT "CHANNEL" . "FILENAME-LIST")

INPUT releases any file previously initialized on the channel, and initializes for input the 
first file specified by the filename-list.
INPUT returns the channel if one was specified, T otherwise. INPUT does not evaluate its arguments.


 (INC CHANNEL ACTION)
INC selects the specified channel for input. The channel NIL selects the teletype. 

If ACTION = NIL then the previously selected input filfe is not released, but only deselected.
If ACTION = T then that file is released, 
making the previously selected channel available.
At the top level, ACTION need not be specified.
The input functions in Ih.j.3 receive input from the selected input channel.
When a file on the selected channel is exhausted, then the next file in the filename-list for the channel 
is initialized and input, until the filename-list is exhausted. Then the teletype is autcrmgticallg selected 
for input and (ERR (QUOTE $EOF$))is called. The use of ERRSET around any functions which accept input therefore
 makes it possible to detect end of file. If no ERRSET is used, control returns to the top
level of LISP. INC evaluates its arguments, and returns the previously selected channel name.
In order t o READ from multiple input sources, separate channels should be initialized by INPUT, 
and INC can then select the appropriate channel t o READ from.
Examples: ;; (At the top level)
  (INC (INPUT SYS: (SMILE . LSP)))
will READ the file SYS: SMIIE .ISP on channel T and reselect the teletype when the file is ended.
  (INC (INPUT FOO DSK: BAZ ZAB) )
will READ the files DSK: BAZ and DSK: ZAB on channel FOO and reselect the teletype after both files are exhausted.

(ERRSET E "F")
ERRSET evaluates the S-expression E and if no error occurs during its evaluation, ERRSET returns (LIST E). If an error occurs, then the
error message will be suppressed if and only if /= NIL, and NIL is returned as the value of ERRSET. If the function ERR is called during evaluation, then no message is printed and ERRSET returns the value returned by ERR.
||#

#+ignore
(defun look-up (word)
  (if (null (setf entry (assocfile word INDICES)))
        (return-from look-up nil))
  (let ((ic (eval (list 'inc (list 'input (gentemp) (second entry) (third entry)) nil))))
    (loop while (nextentry)
          for next = (read)
          when (eq word next)
               do (eval next)
                    (inc ic t)
                    (return t))))
                 


;;; NEXTFILE takes a DSKIN format like (:FOO bax (22 12) A B :SAM (d.lisp))
;;; uses FILESCAN to set the current PPN DEVICE and FILE , opens FILE for input, sets PREV
;;; to the previous channel, closes PREV if CLOSE is non--nil and returns the rest of the file list;

#+ignroe
(defun nextfile (files close)
  (if (null files) (return-from nextfile nil))
  (let* ((temp (filescan files DEV PPN))
         (files (cdr temp))
         (this (car temp)))
    (setf DEV (first this))
    (setf PPN (second this))
    (cond ((atom (setf FILE (third this)))
           (setf *file (cons FILE 'idx))))
    (setf PREV (eval (list 'inc (list 'input (gentemp) DEV PPN FILE))))
    files))


;;; FILESCAN scans down the file-list looking for the first file-name, resetting the device
;;; and PPN along the way as necessary

#+ignore
(defun filescan (file-list device ppn)
  (if (null file-list) (return-from filescan (cons (list DEVICE PPN nil) nil)))
  (loop while file-list
        for temp in file-list
        do (cond ((atom temp)
                  (cond ((equal (car (last (explode temp))) ':)
                         (setf DEVICE temp))
                        (t (return (cons (list DEVICE PPN TEMP) FILE-LIST)))))
                 ((and (numberp (car temp)) (numberp (second temp)))
                  (setf PPN temp)
                  )
                 (t (return (cons (list DEVICE PPN TEMP) file-list))))
        finally (return (cons (list DEVICE PPN TEMP) file-list)))
  )






;;; ASSOCFILE reads s-expressions from the files in FILELIST until one is found
;;; whose CAR is X; i.e., a file is like one big association list but without parens at the outer level

;;; THIS WILL NEED TO BE REWRITTEN for I/O model
#+ignore
(defun ASSOCFILE (x filelist)
  (setf PPN '(0 0))
  (setf DEV :dsk)
  (setf filelist (nextfile filelist nil))
  (setf oldi PREV)
  (prog ()
     :checkf
       (cond ((incontents x (if (atom file) file (car file)))
              (go :nexte)))
     :nextf
       (cond (filelist
                    (setf filelist (nextfile filelist t))
                    (go :checkf))
             (t (inc oldi t)
                (return nil)))

     :nexte
       ;;; in UCI-LISP, errset catches errors

       (cond ((atom (errset (setq entry (read)) nil))
                    (go nextf)))
       (cond ((and (consp entry) (equal x (car entry)))
              (inc oldi t)
              (return entry)))
       ))



             
                   
