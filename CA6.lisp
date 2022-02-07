;; CA6.lisp W.D. Johnson 10/79 Revision of Larry Birnbaum's code
;;          Last changed - M. Burstein 11/8/79
;; Functions for use in tests of requests

;; IF-FIND is called just like LAMBDA: (IF-FIND <args> <S-expression>).
;; it builds a lambda-expression, which is applied in turn to the
;; conceptualizations on the :C-LIST, from the most recently added to the
;; least recently added (i.e., back to fron), and returns the first for which
;; the lambda-expression returns a non-NIL value, else returns NIL
;; and embedded check is added to the lambda-expression

(DF IF-FIND (L-BODY)
  (LET (VAR (CAAR L-BODY) TEMP NIL)
    (SETQ TEMP
      (SOME
        (LIST 'LAMBDA (LIST VAR)
          (CONS 'AND
            (CONS (LIST 'NOT (LIST 'GET VAR ''EMBEDDED))
              (CDR L-BODY))))
          :C-LIST))
    (COND (TEMP (CAR TEMP)))))

;; Returns T if the gap is filled, NIL if it is not.
(DE IS-FILLED (PATH CON)
  (NOT (EQUAL (GET-ROLE-VALUE PATH CON) @(NIL))))

;; This is used to look ahead.
;; it will be made sensitive to clause boundaries.
(DE CHECK-NEXT-ITEM ()
  (AND (NOT (CLAUSE-BREAK-POINT :NEXT-WORD))
    :NEXT-WORD))

;; Check if we're at a break in the input stream
(DE CLAUSE-BREAK-POINT (WORD)
  (EQ WORD 'PRD))

;; Checks to see if word could mark the end of a clause;
(DE CLAUSE-POINT (WORD)
  (MEMQ WORD @(CMA PRD WHEN SO)))

;; This applies the predicate to OBJ and returns the resulting value
;; First looks under TYPE property and then slots; else hack groups
;; else check CAR of object

(DE FEATURE (OBJ PRED)
  (COND ((GET PRED 'EXPR)
          (APPLY PRED (LIST OBJ)) )
    ((GROUP OBJ)
      (OR (FEATURE (GET-ROLE-FILLER @(MEMBER) OBJ) PRED)
        (FEATURE (GET-ROLE-FILLER @(MEMBER1) OBJ) PRED)
        (FEATURE (GET-ROLE-FILLER @(MEMBER2) OBJ) PRED)))
    ( (EQ (CAR (ATOM-EVAL OBJ)) PRED))
    (T (EQ (GET (CAR ATOM-EVAL OBJ)) 'TYPE) PRED)))

;; The following assume :C-LIST is normally reverse-ordered. (last in is first)

;; PRECEDES returns a non-NIL value iff C1 precedes C2 on the :C-LIST
;; i.e, C1 was built before C2.
(DE PRECEDES (C1 C2)
  (AND (NOT (EQ C1 C2)) (MEMB C1 (MEMB C2 :C-LIST))))

;; FOLLOWS returns a non-NIL value iff C1 follows C2 on the :C-LIST
;; i.e, C1 was built after C2 was.
(DE FOLLOWS (C1 C2)
  (AND (NOT (EQ C1 C2)) (MEMB C2 (MEMB C1 :C-LIST))))

;; IM-FOLLOWS returns a non-NIL value iff C1 immediately follows C2 on the :C-LIST.
(DE IM-FOLLOWS (C1 C2)
  (EQ C2 (CADR (MEMB C1 :C-LIST))))

;; Returns a non-NIL value iff X is a punctuation; applied at the lexical level; obsolescent
(DEFPROP BREAKPOINT
  (LAMBDA (X) (MEMQ X @(PERIOD QMARK)))
  EXPR)

;; Check if a work is a verb but not a past participle
(DE NOT-PPART (X)
  (AND
    (MEMB 'VERB (GET X 'POS:))
    (MEMB 'PRESPART (GET X 'MORPH:))))  ; note that this solution is plausible from
                                        ; a speech-processing standpoint, i.e.
                                        ; present tense should default to PPART

;; THESE DON'T SEARCH CORRECTLY ***** PLEASE NOTE *******
;; Check if X is subject of Y (is sensitive to passive)
(DE IS-SUBJ (X Y)
  (COND
    ((MEMB 'PASSIVE-FLAG :FLAGS)
      (FOLLOWS X Y))
    (T (PRECEDES X Y))))

;; Check if X is object of Y
(DE IS-OBJ (X Y)
  (COND
    ((MEMB 'PASSIVE-FLAG :FLAGS)
      (PRECEDES-R X Y))
    (T (FOLLOWS-R X Y))))

;; FOLLOWS-R first checks for an item which follows, then for an item which precedes and is unbound
(DE FOLLOWS-R (X Y)
  (OR
    (FOLLOWS X Y)
    (AND (NOT (GET X 'EMBEDDED)) (PRECEDES X Y))))

;; PRECEDES-R checks for preceding unbound items

(DE PRECEDES-R (X Y)
  (AND (NOT (GET X 'EMBEDDED) (PRECEDES X Y))))
