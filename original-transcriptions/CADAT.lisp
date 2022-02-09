(in-package :CDPARSER)

;;; CADAT.lisp 10/79 W.L. Johnson Data for CA

;;; (defprop s v p) from standford lisp
(defmacro defprop (s v p)
  `(progn (setf (get ',s ',p) ',v)
          ',s))

; (deftype word (props :post :morph)) ;; not sure how to translate this. - will see where it is needed
(defprop physobj pp type)
(defprop money pp type)
(defprop ref pp type)
(defprop wife-relation interp-relation type)
(defprop wife-relation ((actor . wife) (recip . husband)) slots)
(defprop child-relation interp-relation type)


  
  
