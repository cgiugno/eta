;; *ETA-SCHEMA*: development version 6
;;
;; Dialogue for blocks world conversation 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defparameter *eta-schema*

'(event-schema :header (((set-of ^me ^you) have-eta-dialog.v) ** ?e)
;``````````````````````````````````````````````````````````````````````
; Blocks world conversation. An expected blocks world dialogue consists
; of the agent repeatedly asking the user if they have a spatial question
; to ask, followed by a question from the user, followed by an appropriate
; response by the agent. This repeats until the user says something interpreted
; as a goodbye, and is possibly preempted by some "smalltalk" questions
; (currently disabled).
;

:episodes (

  ; David has question-answering dialogue.
  ?e1 ((set-of ^me ^you) have-question-answering-dialogue.v)

)

)) ; END defparameter *eta-schema*



;````````````````````````````````````````````````````````
; Create empty hash tables for semantics,
; gist-clauses, and topic-keys
;
(setf (get '*eta-schema* 'semantics) (make-hash-table))
(setf (get '*eta-schema* 'gist-clauses) (make-hash-table))
(setf (get '*eta-schema* 'topic-keys) (make-hash-table))



;````````````````````````````````````````````````````````
; EL Semantics - Not yet used
;
(mapcar #'(lambda (x)
      (store-output-semantics (first x) (second x) '*eta-schema*))
  '()
) ; END mapcar #'store-output-semantics



;````````````````````````````````````````````````````````
; Gist clauses
;
(mapcar #'(lambda (x) 
      (store-output-gist-clauses (first x) (second x) '*eta-schema*))
  '()
) ; END mapcar #'store-output-gist-clauses



;````````````````````````````````````````````````````````
; Topic keys
;
(mapcar #'(lambda (x) 
      (store-topic-keys (first x) (second x) '*eta-schema*))
  '()
) ; END mapcar #'store-topic-keys
