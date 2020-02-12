;; *medicine-schema*: development version 6
;;
;; TODO
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defparameter *medicine-schema*

'(Event-schema (((set-of me you) discuss-medicine.v) ** ?e)
;```````````````````````````````````````````````````````````
; Blocks world conversation. An expected blocks world dialogue consists of the agent repeatedly asking
; the user if they have a spatial question to ask, followed by a question from the user, followed by
; an appropriate response by the agent. This repeats until the user says something interpreted as a goodbye,
; and is possibly preempted by some "smalltalk" questions (currently disabled).
;

:episodes 

?a1. (Me say-to.v you '(I would like a refill \.))

?a2. (you reply-to.v ?a1.)

?a3. (Me react-to.v ?a2.)

)) ; END defparameter *medicine-schema*



;````````````````````````````````````````````````````````
; Store schema variable name under header in *schemas*
;
(store-schema-name 'discuss-medicine.v '*medicine-schema*)



;````````````````````````````````````````````````````````
; Create empty hash tables for semantics,
; gist-clauses, and topic-keys
;
(setf (get '*medicine-schema* 'semantics) (make-hash-table))
(setf (get '*medicine-schema* 'gist-clauses) (make-hash-table))
(setf (get '*medicine-schema* 'topic-keys) (make-hash-table))



;````````````````````````````````````````````````````````
; EL Semantics - Not yet used
;
(mapcar #'(lambda (x)
      (store-output-semantics (first x) (second x) '*medicine-schema*))
  '()
) ; END mapcar #'store-output-semantics



;````````````````````````````````````````````````````````
; Gist clauses
;
(mapcar #'(lambda (x) 
      (store-output-gist-clauses (first x) (second x) '*medicine-schema*))
  '(
    (?a1.  ((I would like a refill of medicine \.)))
  )
) ; END mapcar #'store-output-gist-clauses



;````````````````````````````````````````````````````````
; Topic keys
;
(mapcar #'(lambda (x) 
      (store-topic-keys (first x) (second x) '*medicine-schema*))
  '(
    (?a1.  (medicine))
  )
) ; END mapcar #'store-topic-keys
