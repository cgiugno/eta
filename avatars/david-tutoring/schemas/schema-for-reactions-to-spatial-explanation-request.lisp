
;; Oct, 18/19: define the schema *reactions-to-spatial-explanation-request*
;; ================================================================
;; 
;; 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *reactions-to-spatial-explanation-request*

  ;; '(event-schema :header ((^me react-to-spatial-question ?var ?earlier-ques) ** ?e)
  ; ?var would be the name of the (you say-to.v me '...) action
  ; that now has a ULF version of the question attached to it;
  ; the ?earlier-ques would be the previous question asked by the
  ; user -- this would be for possible referent interpretation,
  ; especially if it alse has an attached ULF; but maybe this
  ; wouldn't be an explicit argument, but rather attached
  ; somewhere, & reachable by searching backwards...
  '(event-schema :header ((^me react-to-spatial-explanation-request ?ulf ?earlier-ques) ** ?e)
  ;`````````````````````````````````````````````````````````````````````````````````````````````
  ; NOTE: Currently using ?ulf rather than ?var, since there is no way to provide
  ; an action name as an argument when a schema is selected during pattern transduction
  ; TODO: Add this functionality and then add ?var back to header
    :episodes (
              ?e1 (^me seek-answer-from.v |Spatial-QA-Server| ?ulf)
               ; this would send the ulf (obtained from the properties
               ; of the actual name replacing ?var) to an appropriate
               ; file, monitored by the Spatial-QA-Server; the server
               ; would empty the file after reading it;
               ; Currently variable given should be '?ans+alternates if expect
               ; to recieve list of answer and then alternates, or should be given
               ; as '?ans if expect to recieve only answer.
              ?e2 (^me receive-answer-from.v |Spatial-QA-Server| ?ans+alternates)
               ; the value of ?ans+alternates would be read off from a file
               ; to which Spatial-QA-Server sends the answer (with weighted
               ; alternates); once ; the answer is read off, the file would
               ; be emptied.
              ?e3 (^me conditionally-say-to.v ^you (main-answer.f ?ans+alternates))
               ; here ?ans is split off from ?ans+alternates;
               ; "conditionally say to you" would normally expand
               ; into just (^me say-to.v you '?ans); but I'm thinking
               ; of keeping the door open to something more complex,
               ; in cases where Georgiy's system provides multiple,
               ; weighted possibilities, in which case one might
               ; instantiate a subplan for generating a main answer
               ; but also mention alternates (attached as property
               ; to ?e3, I suppose).
    )

)) ; END parameter *reactions-to-spatial-question*


(setf (get '*reactions-to-spatial-question* 'semantics) (make-hash-table))
 ; To fill this in, EL formulas would need to be derived from
 ; Eta reactions (not yet used). This would be rather unlike
 ; the explicit 'store-output-semantics' used in *Eta-schema*
 ; (see "Eta5-schema.lisp").


(setf (get '*reactions-to-spatial-question* 'gist-clauses) (make-hash-table))
 ; Much the same comment as above applies -- something other than
 ; the straightforward 'store-output-gist-clauses' used in
 ; the *Eta-schema* code would be needed.

(setf (get '*reactions-to-spatial-question* 'topic-keys) (make-hash-table))

