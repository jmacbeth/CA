(in-package :cdparser)

;;; CA8.lisp contains named requests

(defvar NAME-REQ
  '(request
    (test t)
    (actions
     (let ((name-group nil) pointer)
        ;; to check if current word is a name. This is here to make the request general enough
        ;; to be called by either names or titles
        (cond ((or (has-pos WORD 'name)
                   (has-pos WORD nil))
               (setf name-group (cons WORD name-group)))) ;; note: name-group was nil till here so = (list word)
                   
        (loop for nxtitm = (check-next-item)   ;; rewritten from prog loop
              while (and nxtitm
                         (or (has-pos nxtitm 'name)
                             (null (get nxtitm :pos))))
              do (get-next-item)
                 (setf name-group (cons WORD name-group))
              finally (if (null name-group) (return nil))
                      (setf pointer (add-con '(*pp* ppclass (#human) last-name (nil) first-name (nil))
                                             nil nil))
                      (fill-gap '(last-name) pointer (build-cd (list (car name-group)) nil nil))
                      (unless (eql 1 (length name-group))
                        (fill-gap '(first-name) pointer (build-cd (last name-group)) nil nil))
                      (end-noun-phrase)
                      (consider-all-requests))))))


                             
