;; *ETA-SCHEMA*: development version 6
;;
;; TODO
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defparameter *eta-schema*

'(Event-schema (((set-of me you) have-eta-dialog.v) ** ?e)
;```````````````````````````````````````````````````````````
; An Eta dialogue focused around a patient-doctor interaction: after an introduction, the
; doctor may initiate conversation with a question. Otherwise, the patient has a list of
; questions on his agenda to ask the doctor (each one may spiral to various sub-dialogues
; before returning to the overall conversation track).

:episodes 

?a1. (Me say-to.v you '(Hi\, it\'s nice to met you\. I\'ve just moved back here and I was
                          doing pretty well after the radiation\, but now this pain seems
                          to be getting worse\.))

?a2. (You reply-to.v ?a1.)

?a3. (Me react-to.v ?a2.)

?a4. (Me say-to.v you '(I have a list of questions I would like to discuss with you now\.))

?a5. (Me have-subdialogue.v you (I would like a refill of my medicine \.)
                                ((I would like a refill of medicine \.)))

?a6. (Me have-subdialogue.v you (Can you tell me what you think my prognosis is going to be ?)
                                ((What is my prognosis ?)))

;; ?a5. (Me discuss-medicine.v you)

;; ?a6. (Me discuss-prognosis.v you)

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
