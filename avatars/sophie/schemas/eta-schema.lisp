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
;
; 1. SOPHIE acted as a real cancer patient.
; 2. SOPHIE was able to show her concerns.
; 3. I did not understand what SOPHIE was saying about her prognosis.
; 4. I was given at one opportunity where SOPHIE needed a clarification.
; 5. I was able to discuss the treatment options with SOPHIE.
; 6. During the conversation I think there was chance of being empathetic.
; 7. I ended the conversation with a positive note without giving false hope.
; 8. I think SOPHIE understood her prognosis.
;
; A couple low cognitive load questions to start with
; Medicine/addition concerns, "how do I know if it's working?"
; "Pain medicine doesn't work, need something stronger?"
; "What are my choices at this point?"
; If chemotherapy not mentioned, ask about it
; "Be honest - what kind of time are we looking at?"
; If hospice not mentioned, ask about it
; What to tell family?
;

:episodes 

; Should feedback be explicitly mentioned here?
?a1. (Me say-to.v you '(Hi\, my name is Sophie\. I am a computer character\. I may sound choppy\, but I am still able to have
                        a conversation with you\. I just moved back into the area from Florida about two weeks ago\. I was recently
                        diagnosed with lung cancer\, and finished radiation about a month ago\. My pain seemed to be under
                        control for a while\, but it seems to be getting worse now\. I\'m meeting with you today to help
                        get some questions answered about my options and my future\.))


?a10. (Me say-to.v you '(One thing I\'ve noticed in the last few weeks is that I haven\'t been sleeping very well\. Most nights I
                        have to take medication for my pain\. I\'m not sure if it\'s only the pain\, but I keep waking up at night\.
                        Do you know why that is?))

?a11. (You reply-to.v ?a10.)

?a12. (Me react-to.v ?a11.)
; hmm, I see.


?a20. (Me say-to.v you '(I just started on my new pain medication recently\. How long will it be before I know if it\'s working?))
; doctor might ask for clarification of what medicine patient is using

?a21. (You reply-to.v ?a20.)

?a22. (Me react-to.v ?a21.)


; if doctor doesn't give satisfying answer before
?a30. (Me say-to.v you '(Doctor\, I\'m in a lot of pain\, and the Lortab just isn\'t working\. I think maybe I need something
                     stronger for my pain\.))
; "So my recommendation is you take a stronger medicine and that there is no ceiling on the dose of narcotic"

?a31. (You reply-to.v ?a30.)

?a32. (Me react-to.v ?a31.)


; Should I have this? As a way to transition into the "harder" topics?
?a35. (Me say-to.v you '(Can you explain to me what my test results show? How should I interpret them?))

?a36. (You reply-to.v ?a35.)

?a37. (Me react-to.v ?a36.)


?a40. (Me say-to.v you '(What are my choices at this point?))
; doctor might reply something about prognosis in response here

?a41. (You reply-to.v ?a40.)

?a42. (Me react-to.v ?a41.)


?a50. (Me say-to.v you '(I want you to be honest with me\. What kind of time do you think we\'re looking at?))
; doctor might lay out a "best case/worst case" scenario, e.g. Transcript 2

?a51. (You reply-to.v ?a50.)

?a52. (Me react-to.v ?a51.)


; if chemotherapy not already mentioned by doctor
?a60. (Me say-to.v you '(My previous doctor mentioned something about chemotherapy\, but he said to wait to see how
       things go after the radiation\. Do you think I need chemotherapy?))

?a61. (You reply-to.v ?a60.)

?a62. (Me react-to.v ?a61.)


; if hospice not already mentioned by doctor
?a70. (Me say-to.v you '(You know\, I really just prefer to be comfortable at this point\. Is hospice care an option?))

?a71. (You reply-to.v ?a70.)

?a72. (Me react-to.v ?a71.)


?a80. (Me say-to.v you '(I haven\'t told my family everything yet\. I wanted to wait to talk to you first\. What should I say to them?))

?a81. (You reply-to.v ?a80.)

?a82. (Me react-to.v ?a81.)


?a300. (Me say-to.v you '(Thank you for taking the time to meet with me today\. It was helpful to learn more
                          about my options\. I hope we can meet again soon to discuss further\. Bye\.))






; A couple low cognitive load questions to start with
; Medicine/addition concerns, "how do I know if it's working?"
; "Pain medicine doesn't work, need something stronger?"
; "What are my choices at this point?"
; If chemotherapy not mentioned, ask about it
; "Be honest - what kind of time are we looking at?"
; If hospice not mentioned, ask about it
; What to tell family?



;; ?a2. (You reply-to.v ?a1.)

;; ?a3. (Me react-to.v ?a2.)

;; ?a4. (Me have-subdialogue.v you (The pain was pretty much under control for a while\, but in the past week it has been more difficult.
;;                                  It used to be in my back and left side of my chest\, but now it\'s in my shoulder blade too\, and on
;;                                  the other side from where it started\.)
;;                                 ((My pain has recently been getting worse \.)))

;; ?a5. (Me have-subdialogue.v you (I\'ll never forget what happened before this\. When the pain got really bad\, I first went to my regular doctor
;;                                  but he didn\'t find anything and he thought I just had a bad cold\. But the pain and cough got so bad
;;                                  over the next few days\, I went to the E.R. on the weekend\. At the E.R. they thought it was just pneumonia
;;                                  and gave me antibiotics and the Lortab for pain\. When they told me how much I weighed\, I realized that
;;                                  I had lost fifteen pounds over the course of the past six months\. A couple of days after I went to the ER\,
;;                                  I made appointments to go for a bunch of tests\. That took a couple of weeks\. I had to get a bone scan\,
;;                                  and I was sent to a lung doctor for a biopsy\. After all that\, the lung doctor told me I had lung cancer\.)
;;                                 ((The E.R. gave me Lortab for pain \.) (I got my diagnosis after visiting a lung doctor \.)))

;; ?a6. (Me have-subdialogue.v you (I had radiation treatment for about five weeks\, except weekends\. That finished six weeks ago\.)
;;                                 ((I had radiation treatment for five weeks \.)))

;; ?a7. (Me have-subdialogue.v you (Recently\, I haven\'t been sleeping very well\. I am waking up at night\. Most nights I have to take my pain medication
;;                                 before falling back to sleep\.)
;;                                 ((I have not been sleeping well \.)))

;; ?a8. (Me have-subdialogue.v you (I\'m a little bit worried because the medication doesn\'t really seem to be working anymore\. Do you know why ?)
;;                                 ((Why isn\'t the pain medication working anymore ?)))

;; ?a100. (Me say-to.v you '(I would like to go through some questions with you now\, if that\'s okay\.))

;; ?a101. (Me have-subdialogue.v you (What are my choices at this point ?)
;;                                   ((What are my options ?))) ;*

;; ?a102. (Me have-subdialogue.v you (What kind of time are we looking at ?)
;;                                   ((What is my prognosis ?))) ;*

;; ?a103. (Me have-subdialogue.v you (While I\’m here\, can I get a refill on my cholesterol medicine ?)
;;                                   ((Can I have a refill of my cholesterol medicine ?))) ;*

;; ?a200. (Me have-subdialogue.v you (What should I do to schedule and prepare for a follow up appointment ?)
;;                                   ((How should I schedule a follow up appointment ?))) ;*

;; ;; ?a5. (Me have-subdialogue.v you (I would like a refill of my medicine \.)
;; ;;                                 ((I would like a refill of medicine \.)))

;; ;; ?a6. (Me have-subdialogue.v you (Can you tell me what you think my prognosis is going to be ?)
;;                                 ;; ((What is my prognosis ?)))

;; ;; ?a5. (Me discuss-medicine.v you)

;; ;; ?a6. (Me discuss-prognosis.v you)

;; ; Discuss steps for future contact

;; ?a300. (Me say-to.v you '(Thank you for taking the time to meet with me today\. It was helpful to learn more
;;                           about my options\. I hope we can meet again soon to discuss further\. Bye\.))

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
    (?a10. ((Why have I not been sleeping well ?)))
    (?a20. ((How will I know if my pain medication is working ?)))
    (?a30. ((Can I have a stronger pain medication ?)))
    (?a35. ((How should I interpret my test results ?)))
    (?a40. ((What are my choices for treatment ?)))
    (?a50. ((What is my prognosis ?)))
    (?a60. ((Do I need chemotherapy ?)))
    (?a70. ((Should I get hospice care ?)))
    (?a80. ((What should I tell my family ?)))
    ;; (?a1.  ((It is nice to meet you \.) (I\'ve just moved back to Rochester \.)
    ;;         (My pain has recently been getting worse \.)))
  )
) ; END mapcar #'store-output-gist-clauses



;````````````````````````````````````````````````````````
; Topic keys
;
(mapcar #'(lambda (x) 
      (store-topic-keys (first x) (second x) '*eta-schema*))
  '(
    (?a10. (sleep))
    (?a20. (medicine-working))
    (?a30. (stronger-medicine))
    (?a35. (test-results))
    (?a40. (choices))
    (?a50. (prognosis))
    (?a60. (chemotherapy))
    (?a70. (hospice))
    (?a80. (inform-family))
    ;; (?a1.  (introduce))
    ;; (?a4.  (pain-description))
    ;; (?a5.  (diagnosis-details))
    ;; (?a6.  (radiation-treatment))
    ;; (?a7.  (sleep))
    ;; (?a8.  (physical-dependence))
  )
) ; END mapcar #'store-topic-keys
