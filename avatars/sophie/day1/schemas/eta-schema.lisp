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
; 1. I'm here because I heard you had an update on my condition. What is it?
;
; 2. What exactly does this diagnosis mean for me?
;
; 3. The pain has gotten stronger and spread to more areas than it was at first. Could I have a different pain medication?
;
; 4. What are my treatment options at this point? I've already tried radiation, and my previous doctor mentioned chemotherapy.
;
; 5. What kind of time do you think we're looking at?
;
; 6. Is it too early to consider comfort care?
;
; 7. What is my next step? If the worst happens, how do I make sure I have my affairs in order?
;
; 8. How is my life going to change?
;
; 9. Haven't told family everything yet. What should I say to them?
;
; 10. Follow up
;

:episodes 

?a1. (Me say-to.v you '(Hi\, my name is Sophie\. I am a computer character\. I may sound choppy\, but I am still able to have
                        a conversation with you\. I just moved back into the area from Florida about two weeks ago\. I was recently
                        diagnosed with lung cancer\, and finished radiation about a month ago\.))


;; 1. (test-results)
?a10. (Me say-to.v you '(I\'m meeting with you today because my CT scan results just came back\. And\, to be honest\, I barely know what a CT scan is\. Could you help me understand what my test and test results mean?))
?a11. (You reply-to.v ?a10.)
?a12. (Me react-to.v ?a11.)

;; 5. (prognosis)
;; NOTE: may be obviated by previous replies.
?a20. (Me say-to.v you '(I want you to be honest with me\. With a diagnosis like this\, what do you think are my chances of survival?))
?a21. (You reply-to.v ?a20.)
?a22. (Me react-to.v ?a21.)

;; 5. (diagnosis-terminology)
;; NOTE: may be obviated by previous replies.
?a30. (Me say-to.v you '(When I read about cancer\, I see a lot of terms I don\'t understand like metastatic\, stage\, and palliative care\. To be honest\, it makes me feel a little clueless\. Could you explain to me what some of these things mean?))
?a31. (You reply-to.v ?a30.)
?a32. (Me react-to.v ?a31.)

;; 3. (sleep-poorly)
;; NOTE: may be obviated by previous replies.
?a35. (Me say-to.v you '(You know\, it might be related to my diagnosis\, but the last few weeks I haven\'t been sleeping very well\. I
                        try to take my pain medication just before I go to sleep\, but most nights I end up awake anyway\. 
			Do you know why I keep waking up like this?))
?a36. (You reply-to.v ?a35.)
?a37. (Me react-to.v ?a36.)

;; 3. (medicine-request)
;; NOTE: may be obviated by previous replies.
?a40. (Me say-to.v you '(Even during the day\, I have a lot of difficulty getting my pain under control\. I\'ve been trying to take my medication regularly\, but it just hasn\'t been working like it\'s supposed to. Do you think I might need something stronger?))
?a41. (You reply-to.v ?a40.)
?a42. (Me react-to.v ?a41.)

;; 3. (life-change), switch to after five
;; NOTE: may be obviated by previous replies.
?a50. (Me say-to.v you '(Since learning I have cancer\, everything\'s felt so uncertain\. Could you tell me how my life is going to change with this diagnosis?))
?a51. (You reply-to.v ?a50.)
?a52. (Me react-to.v ?a51.)

;; 9. (tell-family)
?a60. (Me say-to.v you '(I haven\'t told my family everything yet\. I wanted to wait to talk to you first\. What should I say to them?))
?a61. (You reply-to.v ?a60.)
?a62. (Me react-to.v ?a61.)


?a300. (Me say-to.v you '(Thank you for taking the time to meet with me today\. Learning about my diagnosis was difficult\, but I appreciate your being here to help me through it\. You\'ve given me a lot to think about and to discuss with my family\. Goodbye\.))

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
    (?e10. ((What do my test results mean ?)))
    (?e20. ((What is my prognosis ?)))
    (?e30. ((What does metastatic mean ?) (What do the stages mean?) (What does palliative care mean?)))
    (?e35. ((Why have I not been sleeping well ?)))
    (?e40. ((Can I have a stronger pain medication ?)))
    (?e50. ((How will my life change ?)))
    (?e60. ((What should I tell my family ?)))
  )
) ; END mapcar #'store-output-gist-clauses



;````````````````````````````````````````````````````````
; Topic keys
;
(mapcar #'(lambda (x) 
      (store-topic-keys (first x) (second x) '*eta-schema*))
  '(
    (?e10. (test-results))
    (?e20. (prognosis))
    (?e30. (diagnosis-terminology))
    (?e35. (sleep-poorly))
    (?e40. (medicine-request))
    (?e50. (life-change))
    (?e60. (tell-family))

  )
) ; END mapcar #'store-topic-keys
