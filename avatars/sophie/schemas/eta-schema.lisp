;; *ETA-SCHEMA*: development version 6
;;
;; TODO
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defparameter *eta-schema*

'(Event-schema (((set-of me you) have-eta-dialog.v) ** ?e)
;```````````````````````````````````````````````````````````
; Blocks world conversation. An expected blocks world dialogue consists of the agent repeatedly asking
; the user if they have a spatial question to ask, followed by a question from the user, followed by
; an appropriate response by the agent. This repeats until the user says something interpreted as a goodbye,
; and is possibly preempted by some "smalltalk" questions (currently disabled).
;

:episodes 

?a1. (Me say-to.v you '(Hi\, it\'s nice to met you\. I\'ve just moved back here and I was
                          doing pretty well after the radiation\, but now this pain seems
                          to be getting worse\.))

?a2. (You reply-to.v ?a1.)
;; ?a2. (You say-to.v me ?gist)

?a3. (Me react-to.v ?a2.)
;; ?a3. (Me reply-to.v ?a2.)

?a4. (Me say-to.v you '(I have a list of questions I would like to discuss with you now\.))

?a5. (Me discuss-medicine.v you)

?a6. (Me discuss-prognosis.v you)

; Discuss steps for future contact

?a7. (Me say-to.v you '(Thank you for the conversation\, goodbye\.))

)) ; END defparameter *eta-schema*



;````````````````````````````````````````````````````````
; Store schema variable name under header in *schemas*
;
(store-schema-name 'have-eta-dialog.v '*eta-schema*)



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
  '(
    (?a1.  ((It is nice to meet you \.) (I\'ve just moved back to Rochester \.)
            (I was doing well after radiation\, but now my pain is getting worse \.)))
  )
) ; END mapcar #'store-output-gist-clauses



;````````````````````````````````````````````````````````
; Topic keys
;
(mapcar #'(lambda (x) 
      (store-topic-keys (first x) (second x) '*eta-schema*))
  '(
    (?a1.  (introduce))
    (?a5.  (medicine))
    (?a6.  (prognosis))
  )
) ; END mapcar #'store-topic-keys
