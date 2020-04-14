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
;
; palleative care
;
; doctor gives scan result, choice between chemotherapy A or chemotherapy B
; doesn't give choice of just focusing on comfort
; patients whose doctors offer comfort care as option more likely to take it
; should give the doctor an opportunity to present it as an option
; "what happens if I don't do chemotherapy?"
; "do you think chemotherapy's really going to help?"
; "do you think it's time for me to start thinking about comfort care?"
;
; prognosis:
; patient asks "what does this mean?"
; doctor says "let's try chemo" => doesn't even address question
; "how much of a difference do you think it'll make in terms of how long I live?"
; "how long do you think I have?"
;
;
; 1. Haven't slept well, have to take medication, keep waking up at night.
;
; 2. Just started on new pain medication recently, how long until I know if it's working?
;
; 3. In a lot of pain, Lortab isn't working. Need something stronger.
;
; 4. What do test results show, how do I interpret them?
;
; 5. What are my choices at this point?
;
; 6. What kind of time do you think we're looking at?
;
; 7. Previous doctor mentioned chemotherapy, mentioned to wait until after radiation. Do you think I need it?
;
; 8. I prefer to be comfortable at this point. Is hospice an option?
;
; 9. Haven't told family everything yet. What should I say to them?
;
; 10. Follow up
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
                        Do you know why I keep waking up like this?))
; doctor might reply saying that cancer has gotten worse, or something.

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


?a35. (Me say-to.v you '(Can you explain to me what my test results mean?))
; doctor might say something irrelevant to prognosis, or they might get into it

?a36. (You reply-to.v ?a35.)

?a37. (Me react-to.v ?a36.)


; if not obviated by "can you explain to me what my test results mean"
?a40. (Me say-to.v you '(I want you to be honest with me\. How long do you think I have?))
; doctor might lay out a "best case/worst case" scenario, e.g. Transcript 2

?a41. (You reply-to.v ?a40.)

?a42. (Me react-to.v ?a41.)


?a50. (Me say-to.v you '(What are my choices at this point?))
; doctor might reply something about prognosis in response here

?a51. (You reply-to.v ?a50.)

?a52. (Me react-to.v ?a51.)


; if chemotherapy not already mentioned by doctor
?a60. (Me say-to.v you '(My previous doctor mentioned something about chemotherapy\, but he said to wait to see how
       things go after the radiation\. Do you think I need chemotherapy?))

?a61. (You reply-to.v ?a60.)

?a62. (Me react-to.v ?a61.)


; if hospice not already mentioned by doctor
?a70. (Me say-to.v you '(You know\, I really just prefer to be comfortable at this point\. Do you think I should
                         start considering comfort care?))

?a71. (You reply-to.v ?a70.)

?a72. (Me react-to.v ?a71.)


?a80. (Me say-to.v you '(I haven\'t told my family everything yet\. I wanted to wait to talk to you first\. What should I say to them?))

?a81. (You reply-to.v ?a80.)

?a82. (Me react-to.v ?a81.)


?a300. (Me say-to.v you '(Thank you for taking the time to meet with me today\. It was difficult to talk about my future\, but comforting to
                          learn more about my options\. I hope we can meet again soon to discuss further\. Bye\.))

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
    (?a35. ((What do my test results mean ?)))
    (?a40. ((What is my prognosis ?)))
    (?a50. ((What are my options for treatment ?)))
    (?a60. ((Do I need chemotherapy ?)))
    (?a70. ((Should I get comfort care ?)))
    (?a80. ((What should I tell my family ?)))
  )
) ; END mapcar #'store-output-gist-clauses



;````````````````````````````````````````````````````````
; Topic keys
;
(mapcar #'(lambda (x) 
      (store-topic-keys (first x) (second x) '*eta-schema*))
  '(
    (?a10. (sleep-poorly))
    (?a20. (medicine-working))
    (?a30. (medicine-request))
    (?a35. (test-results))
    (?a50. (prognosis))
    (?a40. (treatment-option))
    (?a60. (chemotherapy))
    (?a70. (comfort-care))
    (?a80. (tell-family))
  )
) ; END mapcar #'store-topic-keys
