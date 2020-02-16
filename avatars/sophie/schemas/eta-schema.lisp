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

?a1. (Me say-to.v you '(Hi\, it\'s nice to met you\. I just moved back into the area from Florida about two weeks ago\.
                        And for a while I was feeling pretty well after radiation\, but recently the pain seems to be getting worse\.))

?a2. (You reply-to.v ?a1.)

?a3. (Me react-to.v ?a2.)

?a4. (Me have-subdialogue.v you (The pain was pretty much under control for a while\, but in the past week it has been more difficult.
                                 It used to be in my back and left side of my chest\, but now it\'s in my shoulder blade too\, and on
                                 the other side from where it started\.)
                                ((My pain has recently been getting worse \.)))

?a5. (Me have-subdialogue.v you (I\'ll never forget what happened before this\. When the pain got really bad\, I first went to my regular doctor
                                 but he didn\'t find anything and he thought I just had a bad cold\. But the pain and cough got so bad
                                 over the next few days\, I went to the ER on the weekend\. At the ER they thought it was just pneumonia
                                 and gave me antibiotics and the Lortab for pain\. When they told me how much I weighed\, I realized that
                                 I had lost fifteen pounds over the course of the past six months\. A couple of days after I went to the ER\,
                                 I made appointments to go for a bunch of tests\. That took a couple of weeks\. I had to get a bone scan\,
                                 and I was sent to a lung doctor for a biopsy\. After all that\, the lung doctor told me I had lung cancer\.)
                                ((The ER gave me Lortab for pain \.) (I learned about my lung cancer after visiting a lung doctor \.)))

?a6. (Me have-subdialogue.v you (I had radiation treatment for about five weeks\, except weekends\. That finished six weeks ago\.)
                                ((I had radiation treatment for five weeks \.)))


?a100. (Me say-to.v you '(I would like to go through some questions with you now\, if that\'s okay\.))

;; ?a5. (Me have-subdialogue.v you (I would like a refill of my medicine \.)
;;                                 ((I would like a refill of medicine \.)))

;; ?a6. (Me have-subdialogue.v you (Can you tell me what you think my prognosis is going to be ?)
                                ;; ((What is my prognosis ?)))

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
            (My pain has recently been getting worse \.)))
  )
) ; END mapcar #'store-output-gist-clauses



;````````````````````````````````````````````````````````
; Topic keys
;
(mapcar #'(lambda (x) 
      (store-topic-keys (first x) (second x) '*eta-schema*))
  '(
    (?a1.  (introduce))
    (?a4.  (pain-description))
    (?a5.  (diagnosis-details))
  )
) ; END mapcar #'store-topic-keys
