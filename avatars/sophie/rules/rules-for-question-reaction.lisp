(MAPC 'ATTACHFEAT
  '()
)

(READRULES '*reaction-to-question*
; Here we match any important questions which may branch off into a sub-dialogue, i.e. initiate a sub-schema.
; A separate tree is used for matching any less relevant questions, which prompt Eta to give an answer but then
; return to the central track of the conversation.
;
; General format of the below rules:
; 1 (pattern)
;   2 (*have-subdialogue* ((Answer to output \.)
;                          ((Gist clause one \.) (Gist clause two \.)))) (100 :schema+args)
'(
  ; Questions about pain
  1 (Can you tell me about your pain ?)
    2 (*have-subdialogue* ((The pain was pretty much under control for a while\, but in the past week it has been more difficult.
                            It used to be in my back and left side of my chest\, but now it\'s in my shoulder blade too\, and on
                            the other side from where it started\.)
                           ((My pain has recently been getting worse \.)))) (100 :schema+args)
    2 (*have-subdialogue* ((The pain doesn\'t really go into other areas\. It\'s dull and constant\, and aches a lot\. It usually
                            hurts to take deep breathes\.)
                           ((My pain has recently been getting worse \.)))) (100 :schema+args)
    2 (I did have some pain during swallowing after radiation as well\, but that\'s improved a bit\.) (0 :out)
  1 (How do you rate your pain ?)
    2 (*have-subdialogue* ((The pain is about a seven out of ten\. With medication\, it goes down to about a five\.)
                           ((My pain has recently been getting worse \.)))) (100 :schema+args)
  1 (Where is the pain located ?)
    2 (*have-subdialogue* ((The pain is primarily in the left side of my chest\, and in the middle of my back\. Recently\,
                            it also moved to the right side of my chest\.)
                           ((My pain has recently been getting worse \.)))) (100 :schema+args)
  1 (Does it hurt to 2 ?)
    2 (*have-subdialogue* ((It hurts whenever I take a deep breath\. It used to hurt to swallow during radiation\, but that
                            isn\'t as bad now\.)
                           ((My pain has recently been getting worse \.)))) (100 :schema+args)
  ; Questions about anyone here with you
  1 (Is anyone here with you ?)
    2 (*have-subdialogue* ((I\'m staying with my daughter now\. She took the day off to come in today\,
                            but someone called in sick where she works\. So\, she had to go in\. She
                            really wanted to be here\.)
                           ((I am here alone \.)))) (100 :schema+args)

  ; The following two will need modification
  1 (How are you on medicine ?)
    2 (*have-subdialogue* ((I need a refill \.)
                           ((I would like a refill of medicine \.)))) (100 :schema+args)
  1 (What is your prognosis ?)
    2 (*have-subdialogue* ((Can you tell me what kind of time we\'re looking at ?)
                           ((What is my prognosis ?)))) (100 :schema+args)

  1 (0)
    2 *reaction-to-question-minor* (0 :subtree)
))


(READRULES '*reaction-to-question-minor*
; Here we match any question gist clauses which are considered "minor", i.e. off-topic or otherwise
; not expected to branch off into a further sub-dialogue.
'(
  ; Questions about daughter
  1 (Where does your daughter work ?)
    2 (She works as a school nurse in the county school system\.) (0 :out)
  1 (How old is your daughter ?)
    2 (She\'s thirty four\. Turning thirty five in a few months\.) (0 :out)
  ; Questions about family (TODO)
))
