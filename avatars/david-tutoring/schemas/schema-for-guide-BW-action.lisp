;; *guide-BW-action*
;;
;; Dialogue for guiding the user through making a particular BW action.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defparameter *guide-BW-action*

'(Event-schema ((^me guide-BW-action.v ^you ?ka1) ** ?e)
;`````````````````````````````````````````````````````````````````````````````
; Blocks world action instruction; consists of the BW agent guiding the
; user through making a specific action (e.g., a block move). Consists of
; monitoring the user's actions, and checking whether the user's action is
; an instance of ?ka1; otherwise, the agent should issue a correction. The
; agent should allow for the possibility of a question, pause request, or
; termination request during this process as well.

:episodes (

  ;; ?e1 (^me say-to.v ^you '(You try to make a move \.))
  ?e1 (:repeat-until (?e1 finished2.a)

    ; TODO: add "user trying" action, which (in the case of a BW action)
    ; queries the BW system for whether ?ka1 was performed or not. If so,
    ; store ((pair ^you ?e2) instance-of.n ?ka1).
    ?e2 (^you try.v ?ka1)

    (:if ((pair ^you ?e2) instance-of.n ?ka1)

      ; say some variant of "very good."?
      ?e3 (^me commit-to-STM.v (that (?e1 finished2.a)))
    
    :else

      ; TODO: get new next step by querying the BW system
      ?e4 (^me issue-correction-to.v ^you ?ka1)
    
    )

  )

)

)) ; END defparameter *guide-BW-action*



;````````````````````````````````````````````````````````
; Store schema variable name under header in *schemas*
;
(store-schema-name 'guide-BW-action.v
                  '*guide-BW-action*)



;````````````````````````````````````````````````````````
; Create empty hash tables for semantics,
; gist-clauses, and topic-keys
;
(setf (get '*guide-BW-action* 'semantics) (make-hash-table))
(setf (get '*guide-BW-action* 'gist-clauses) (make-hash-table))
(setf (get '*guide-BW-action* 'topic-keys) (make-hash-table))



;````````````````````````````````````````````````````````
; EL Semantics - Not yet used
;
(mapcar #'(lambda (x)
      (store-output-semantics (first x) (second x) '*guide-BW-action*))
  '()
) ; END mapcar #'store-output-semantics



;````````````````````````````````````````````````````````
; Gist clauses
;
(mapcar #'(lambda (x) 
      (store-output-gist-clauses (first x) (second x) '*guide-BW-action*))
  '()
) ; END mapcar #'store-output-gist-clauses



;````````````````````````````````````````````````````````
; Topic keys
;
(mapcar #'(lambda (x) 
      (store-topic-keys (first x) (second x) '*guide-BW-action*))
  '()
) ; END mapcar #'store-topic-keys
