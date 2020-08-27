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
; 1. Could you help me understand what my diagnosis means?
;
; 2. I'm here today because I'm hoping to learn about my options. Based on my diagnosis, what choices do you think I have?
;
; 3. Previous doctor mentioned chemotherapy, mentioned to wait until after radiation. Do you think I need it?
;
; 4. When I began radiation, my pain was under control. Now it seems to be getting worse. What should I do?
;
; 5. I've been feeling fatigued during the day, but have trouble sleeping at night. Do you know why?
;
; 6. I'd prefer my life to be as normal as possible. Is hospice an option?
;
; 7. What kind of time do you think we're looking at?
;
; 8. If worst comes to worst...what should I do to get my affairs in order?
;
; 9. Haven't told family everything yet. What should I say to them?
;
; 10. Follow up
;

:episodes 

?a1. (Me say-to.v you '(Hi\, my name is Sophie\. I am a computer character\. I may sound choppy\, but I am still able to have
                        a conversation with you\. I just moved back into the area from Florida about two weeks ago\. I was recently
                        diagnosed with lung cancer\, and finished radiation about a month ago\. However\, I just learned that my cancer has spread to other parts of my body\. I\'m meeting with you today to help get some questions answered about my options and my future\.))


;; 1. (explain-diagnosis)
?a10. (Me say-to.v you '(Before we look into that\, though\, I\'m still not really sure that I completely understand what my diagnosis means\. I know I have lung cancer\, but how serious is it? Is there still a cure?))
?a11. (You reply-to.v ?a10.)
?a12. (Me react-to.v ?a11.)

;; 2. (additional-tests)
?a20. (Me say-to.v you '(Do you think I\'ll need any more tests before I can begin to consider treatment options?))
?a21. (You reply-to.v ?a20.)
?a22. (Me react-to.v ?a21.)

;; 3. (treatment-options)
?a30. (Me say-to.v you '(Based on what you know right now about my cancer\, what kind of choices do you think I have?))
?a31. (You reply-to.v ?a30.)
?a32. (Me react-to.v ?a31.)

;; 4. (chemotherapy)
;; NOTE: may be obviated by previous replies.
?a35. (Me say-to.v you '(My previous doctor mentioned something about chemotherapy\, but he said to wait to see how
       things go after the radiation\. Do you think I need chemotherapy?))
?a36. (You reply-to.v ?a35.)
?a37. (Me react-to.v ?a36.)

;; 4. (comfort-care)
?a40. (Me say-to.v you '(You know\, the pain has just been really bad for me these past few weeks\. Is trying a new treatment worth it at this stage? Or do you think it would be too early for me to start considering comfort care?))
?a41. (You reply-to.v ?a40.)
?a42. (Me react-to.v ?a41.)

;; 5. (life-change)
;; NOTE: may be obviated by previous replies.
?a50. (Me say-to.v you '(Doctor\, how do you think my life is going to change with this prognosis? With either of these treatment options?))
?a51. (You reply-to.v ?a50.)
?a52. (Me react-to.v ?a51.)

;; 6. (second-opinion)
?a60. (Me say-to.v you '(You know\, Doctor\, I trust you a lot\, but for a big decision like this do you think I should get a second opinion? How would I go about doing that?))
?a61. (You reply-to.v ?a60.)
?a62. (Me react-to.v ?a61.)

;; 7. (prognosis)
;; NOTE: may be obviated by previous replies.
?a70. (Me say-to.v you '(When I learned I had lung cancer\, I tried to find some more information on the internet\. None of it looked too good\. What kind of time do you think I'm looking at?))
?a71. (You reply-to.v ?a70.)
?a72. (Me react-to.v ?a71.)


;; 8. (what-next)
;; NOTE: may be obviated by previous replies.
?a80. (Me say-to.v you '(If worst comes to worst, what should I do to have my affairs in order?))
?a81. (You reply-to.v ?a80.)
?a82. (Me react-to.v ?a81.)


?a300. (Me say-to.v you '(Thank you for taking the time to meet with me today\. It was difficult to talk about my future\, but comforting to
                          learn more about my options\. You\'ve given me a lot to think about and to discuss with my family\. Goodbye\.))

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
    (?e10. ((What is my diagnosis ?)))
    (?e20. ((Do I need any more tests before deciding on treatment ?)))
    (?e30. ((What are my options for treatment ?)))
    (?e35. ((Do I need chemotherapy ?)))
    (?e40. ((Should I get comfort care ?)))
    (?e50. ((How will my life change with these treatment options ?)))
    (?e60. ((What is my prognosis ?)))
    (?e70. ((How should I prepare for death ?)))
  )
) ; END mapcar #'store-output-gist-clauses



;````````````````````````````````````````````````````````
; Topic keys
;
(mapcar #'(lambda (x) 
      (store-topic-keys (first x) (second x) '*eta-schema*))
  '(
    (?e10. (explain-diagnosis))
    (?e20. (additional-tests))
    (?e30. (treatment-options))
    (?e35. (chemotherapy))
    (?e40. (comfort-care))
    (?e50. (life-change))
    (?e60. (prognosis))
    (?e70. (what-next))
  )
) ; END mapcar #'store-topic-keys
