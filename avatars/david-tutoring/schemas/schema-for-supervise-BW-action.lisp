;; *supervise-BW-action*
;;
;; Dialogue for supervising the user through making a particular BW action.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defparameter *supervise-BW-action*

'(event-schema :header ((^me supervise-BW-action.v ^you ?ka1) ** ?e)
;`````````````````````````````````````````````````````````````````````````````
; Blocks world action supervision; consists of the BW agent listening for any
; questions (or other requests) by the user, watching them try to make a move
; towards the goal structure (possibly followed by a follow-up like "is that
; correct?"), and issuing a correction in the case that the user's action isn't
; an instance of ?ka1. The correction simply entails vocalizing ?ka1 with
; corrective phrasing (however, this is suppressed in the case where ?ka1
; involves undoing the previous action, since the user may technically get
; undoing something "wrong" if they don't move a block to exactly the original
; location, but this usually isn't actually an issue in need of correction).

:episodes (

  ?e1 (:repeat-until (?e1 finished2.a)
  
    ; The user may say something to David before attempting to make a move -
    ; namely, a spatial question, or a termination/pause request.
    ?e2 (^you say-to.v ^me ?words1)

    ; Either (3a) the user asks a question, (3b) the user says goodbye, (3c) the
    ; user asks to pause, or (3d) the user is assumed to acknowledge the proposal.
    ?e3 (:try-in-sequence
    
      ; (3a)
      (:if ((^you ask-question.v) * ?e2)

        ; David reacts to the user's question.
        ?e4 (^me react-to.v ?e2))

      ; (3b)
      (:if ((^you say-bye.v) * ?e2)
      
        ; David says goodbye and exits.
        ?e5 (^me react-to.v ?e2)
        ?e6 (^me say-bye.v))

      ; (3c)
      (:if ((^you ask-to-pause.v) * ?e2)
      
        ; David reacts to the pause request and pauses the conversation.
        ?e7 (^me react-to.v ?e2)
        ?e8 ((set-of ^me ^you) pause-conversation.v))

      ; (3d)
      (:else
      
        ; The user attempts to make the proposed move.
        ?e9 (^you try1.v ?ka1)

        ; The user might follow up with a verification question,
        ; like "how's that"?
        ?e10 (^you say-to.v ^me ?words2)

        ; If the user made the correct move...
        ?e11 (:if ((pair ^you ?e9) instance-of.p ?ka1)

          ; If the user explicitly asked about correctness...
          ?e12 (:if ((^you verify-correctness.v) * ?e10)
          
            ; Inform them that their move was correct.
            ?e13 (^me say-to.v ^you '(Good\, that\'s correct\.)))

          ; Exit loop and move on to next step in the plan.
          ?e14 (^me commit-to-STM.v (that (?e1 finished2.a)))

          ; Otherwise, if the user made an incorrect move...
          :else (

            ; Issue a correction by explicitly informing
            ; the user what the correct move should have been.
            ?e20 (^me issue-correction-to.v ^you ?ka1)

            ; Exit loop and move on to next step in the plan
            ; (which, in the case of an incorrect move by the user,
            ; will be the correction/adjustment to that move).
            ; TODO: see note in 'schema-for-guide-BW-action.lisp' - I'm unsure
            ; of whether clarifications should be dealt with in this way (i.e.,
            ; modifications to the subsequent planner inputs), but this is the way
            ; it's done currently with the existing planner capabilities.
            ?e21 (^me commit-to-STM.v (that (?e1 finished2.a))))))))

)

)) ; END defparameter *supervise-BW-action*



;````````````````````````````````````````````````````````
; Store schema variable name under header in *schemas*
;
(store-schema-name 'supervise-BW-action.v
                  '*supervise-BW-action*)



;````````````````````````````````````````````````````````
; Create empty hash tables for semantics,
; gist-clauses, and topic-keys
;
(setf (get '*supervise-BW-action* 'semantics) (make-hash-table))
(setf (get '*supervise-BW-action* 'gist-clauses) (make-hash-table))
(setf (get '*supervise-BW-action* 'topic-keys) (make-hash-table))



;````````````````````````````````````````````````````````
; EL Semantics - Not yet used
;
(mapcar #'(lambda (x)
      (store-output-semantics (first x) (second x) '*supervise-BW-action*))
  '()
) ; END mapcar #'store-output-semantics



;````````````````````````````````````````````````````````
; Gist clauses
;
(mapcar #'(lambda (x) 
      (store-output-gist-clauses (first x) (second x) '*supervise-BW-action*))
  '()
) ; END mapcar #'store-output-gist-clauses



;````````````````````````````````````````````````````````
; Topic keys
;
(mapcar #'(lambda (x) 
      (store-topic-keys (first x) (second x) '*supervise-BW-action*))
  '()
) ; END mapcar #'store-topic-keys
