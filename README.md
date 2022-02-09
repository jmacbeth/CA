# CA - Larry Birnbaum's Conceptual Analyzer

In the late 1970s the Yale AI group developed a set of natual language
interpreters that went straight from surface language (sentences) to
conceptual dependency (CD) semantic notation. For reference, see the book 
"Scripts, Plans, Goals and Understanding" by Roger Schank.

CA was the acronym (for Conceptual Analyzer) one of the systems for
semantic parsing of language into Conceptual Dependency developed at Yale
University by that group between 1976-9. This code was originally developed
by Larry Birnbaum in MLISP and was based on the orignal system ELI (English
Language Interpreter) developed by Chris Riesbeck (see chapter on Micro-ELI
in "Inside Computer Understanding" by R.C.Schank and C.K.Riesbeck and one
on Conceptual Analysis by L. Birnbaum and M. Selfridge). Modifications and
additions to this code were made in 1978-9 by Mark Burstein and Lewis
Johnson, and another very similar system with much more support for compact
vocabulary definitions was nearly contemporaneously developed by Mike Dyer
and used in the BORIS system.

This repo contains code recovered from an original source printout and
manually transcribed by Mark Burstein in January of 2022. It was converted
into CommonLisp from the original UCI Lisp code. This "working" version
(with some rewrites to simplify the word definitions, the messages
produced, and how requests (the interpretation rules within definitions)
are processed.

The code has ONLY been made to work well enough for it to be tested on the
set of sentences in test.lisp.

A lot of unused functions remains to be cleaned out and the commentary
improved. The original code had a number of hooks for use by other project
systems.

== HOW TO LOAD ==

This system was loaded and tested in SBCL.  It relies on ASDF.

To run in SBCL set up a registry entry for this directory or call
(load "<this dir>/cdparser.asd")

then

(asdf::load-system :CDPARSER)
(in-package :CD)

Then test with the sentences in test.lisp, for example,
(in-package :CD)
#<PACKAGE "CDPARSER">
CD> (ca tst6)
Current Input: ((A SMALL TWIN-ENGINE PLANE STUFFED WITH MARIJUANA CRASHED SOUTH OF HERE YESTERDAY))
New sentence is 
(A SMALL TWIN-ENGINE PLANE STUFFED WITH MARIJUANA CRASHED SOUTH OF HERE YESTERDAY) 


======================= Current word: A ========================== 
  Phrase: (A) rest: (SMALL TWIN-ENGINE PLANE STUFFED WITH MARIJUANA CRASHED SOUTH OF HERE YESTERDAY)
ACTIVATE-ITEM-REQUESTS for word A: (REQ-A-1)
REQ-A-1 has fired
Adding CON3 = (*INDEF*)
REQ-A-1 activating new requests: (REQ-A-4)
Begin noun group:
Begin noun group;

======================= Current word: SMALL ========================== 
  Phrase: (A SMALL) rest: (TWIN-ENGINE PLANE STUFFED WITH MARIJUANA CRASHED SOUTH OF HERE YESTERDAY)
Begin noun group:
ACTIVATE-ITEM-REQUESTS for word SMALL: (REQ-SMALL-5)
REQ-SMALL-5 has fired
Adding CON7 = (*LTNORM*)
REQ-SMALL-5 activating new requests: (REQ-SMALL-8)

======================= Current word: TWIN-ENGINE ========================== 
  Phrase: (A SMALL TWIN-ENGINE) rest: (PLANE STUFFED WITH MARIJUANA CRASHED SOUTH OF HERE YESTERDAY)
Begin noun group:
ACTIVATE-ITEM-REQUESTS for word TWIN-ENGINE: (REQ-TWIN-ENGINE-9)
REQ-TWIN-ENGINE-9 has fired
Adding CON11 = (*PP* :CLASS CON12 :NUMBER CON13 :MEMBER CON15)
REQ-TWIN-ENGINE-9 activating new requests: (REQ-TWIN-ENGINE-18)
REQ-SMALL-8 has fired
Inserting CON7 into CON11 at (:SIZE)

======================= Current word: PLANE ========================== 
  Phrase: (A SMALL TWIN-ENGINE PLANE) rest: (STUFFED WITH MARIJUANA CRASHED SOUTH OF HERE YESTERDAY)
End of noun group;
ACTIVATE-ITEM-REQUESTS for word PLANE: (REQ-PLANE-19)
REQ-PLANE-19 has fired
Adding CON21 = (*PP* :CLASS CON22 :TYPE CON23)
REQ-TWIN-ENGINE-18 has fired
Inserting CON11 into CON21 at (:HAS-PART)
REQ-A-4 has fired
A = CON3 found pp CON21 = CON3
Inserting CON3 into CON21 at (:REF)

======================= Current word: STUFFED ========================== 
  Phrase: (A SMALL TWIN-ENGINE PLANE STUFFED) rest: (WITH MARIJUANA CRASHED SOUTH OF HERE YESTERDAY)
ACTIVATE-ITEM-REQUESTS for word STUFFED: (REQ-STUFFED-24)
REQ-STUFFED-24 has fired
Adding CON26 = (*DO* <=> CON27 :ACTOR CON28 :OBJECT CON29 :TO CON30 :FROM CON32 :TIME CON33)
Activating lexical requests (REQ-WITH-34 REQ-WITH-35) bindings ((STR1 . CON26) (STR2) (STR3))

======================= Current word: WITH ========================== 
  Phrase: (A SMALL TWIN-ENGINE PLANE STUFFED WITH) rest: (MARIJUANA CRASHED SOUTH OF HERE YESTERDAY)
Considering lexical requests:
REQ-WITH-34 has fired
REQ-WITH-35 has fired
ACTIVATE-ITEM-REQUESTS for word WITH: NIL
REQ-WITH-36 has fired
Inserting CON26 into CON21 at (:REL)
Begin noun group:
Begin noun group;

======================= Current word: MARIJUANA ========================== 
  Phrase: (A SMALL TWIN-ENGINE PLANE STUFFED WITH MARIJUANA) rest: (CRASHED SOUTH OF HERE YESTERDAY)
End of noun group;
ACTIVATE-ITEM-REQUESTS for word MARIJUANA: (REQ-MARIJUANA-39)
REQ-MARIJUANA-39 has fired
Adding CON41 = (*PP* :CLASS CON42 :TYPE CON43)
REQ-WITH-37 has fired
Inserting CON41 into CON26 at (:OBJECT)

======================= Current word: CRASHED ========================== 
  Phrase: (A SMALL TWIN-ENGINE PLANE STUFFED WITH MARIJUANA CRASHED) rest: (SOUTH OF HERE YESTERDAY)
ACTIVATE-ITEM-REQUESTS for word CRASHED: (REQ-CRASHED-44)
REQ-CRASHED-44 has fired
Adding CON46 = (*DO* <=> CON47 :OBJECT CON48 :PLACE CON49)
REQ-CRASHED-44 activating new requests: (REQ-CRASHED-50 REQ-CRASHED-51)
REQ-CRASHED-50 has fired
Inserting CON21 into CON46 at (:OBJECT)
Begin noun group:
Begin noun group;

======================= Current word: SOUTH ========================== 
  Phrase: (A SMALL TWIN-ENGINE PLANE STUFFED WITH MARIJUANA CRASHED SOUTH) rest: (OF HERE YESTERDAY)
End of noun group;
ACTIVATE-ITEM-REQUESTS for word SOUTH: (REQ-SOUTH-52)
REQ-SOUTH-52 has fired
Adding CON54 = (*SOUTH*)
REQ-SOUTH-52 activating new requests: (REQ-SOUTH-55)
Activating lexical requests (REQ-OF-56) bindings ((STR1 . CON54) (STR2) (STR3) (STR4))

======================= Current word: OF ========================== 
  Phrase: (A SMALL TWIN-ENGINE PLANE STUFFED WITH MARIJUANA CRASHED SOUTH OF) rest: (HERE YESTERDAY)
Considering lexical requests:
REQ-OF-56 has fired
Adding CON57 = (*LOC* :PROX CON58 :DIR CON59)
ACTIVATE-ITEM-REQUESTS for word OF: NIL
REQ-CRASHED-51 has fired
Inserting CON57 into CON46 at (:PLACE)
Begin noun group:
Begin noun group;

======================= Current word: HERE ========================== 
  Phrase: (A SMALL TWIN-ENGINE PLANE STUFFED WITH MARIJUANA CRASHED SOUTH OF HERE) rest: (YESTERDAY)
Begin noun group:
ACTIVATE-ITEM-REQUESTS for word HERE: (REQ-HERE-62)
REQ-HERE-62 has fired
Adding CON64 = (*LOC* :PROX CON65)
REQ-OF-60 has fired
Inserting CON54 into CON57 at (:DIR)
Inserting CON64 into CON57 at (:PROX)

======================= Current word: YESTERDAY ========================== 
  Phrase: (A SMALL TWIN-ENGINE PLANE STUFFED WITH MARIJUANA CRASHED SOUTH OF HERE YESTERDAY) rest: NIL
End of noun group;
ACTIVATE-ITEM-REQUESTS for word YESTERDAY: (REQ-YESTERDAY-66)
REQ-YESTERDAY-66 has fired
Adding CON68 = (*YESTERDAY*)
REQ-YESTERDAY-66 activating new requests: (REQ-YESTERDAY-69)
REQ-YESTERDAY-69 has fired
Inserting CON68 into CON46 at (:TIME)
CA exiting. flags= NIL
CA processed words: (A SMALL TWIN-ENGINE PLANE STUFFED WITH MARIJUANA CRASHED SOUTH OF HERE YESTERDAY) 
  C-LIST = ((CON3) (CON7) (CON11) (CON21) (CON26) (CON41) CON46 (CON54) (CON57) (CON64) (CON68)) 

Result: ((*DO* <=> ($CRASH)
           :OBJECT (*PP* :CLASS (VEHICLE) :TYPE (*AIRPLANE*)
                         :HAS-PART (*PP* :CLASS (GROUP) :NUMBER (*NUM* NUMBER (*2*))
                                         :MEMBER (*PP* :CLASS (STRUCTURE) :TYPE (*ENGINE*))
                                         :SIZE (*LTNORM*)) 
                          :REF (*INDEF*)
                          :REL (*DO* <=> (*PTRANS*)
                                   :ACTOR (NIL)
                                   :OBJECT (*PP* :CLASS (PHYSOBJ) :TYPE (*MJ*))
                                   :TO (*INSIDE* :PART CON21)
                                   :FROM (NIL) :TIME (NIL)))
          :PLACE (*LOC* :PROX (*LOC* :PROX (*HERE*)) :DIR (*SOUTH*))
          :TIME (*YESTERDAY*))) 


C-LIST: ((CON3) (CON7) (CON11) (CON21) (CON26) (CON41) CON46 (CON54) (CON57) (CON64) (CON68)) 
(CON46)


Notes: When it shows C-LIST, this is a list of the concept forms or
fragments as they were constructed. Ones in parens have been incorporated
into others. The one remaining is the top level form printed after
expansion as the Result:

Note also that "small" is attached to "twin-engine" rather than "plane"
based on the relatively unsophistocated rules used to manage that semantic
attachment ambiguity. (see crash-dic.lisp)

- Mark Burstein, February 8, 2022

