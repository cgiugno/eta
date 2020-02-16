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
  1 (is anyone here with you ?)
    2 (*have-subdialogue* ((I\'m staying with my daughter now\. She took the day off to come in today\,
                            but someone called in sick where she works\. So\, she had to go in\. She
                            really wanted to be here\.)
                           ((I am here alone \.)))) (100 :schema+args)

  ; The following two will need modification
  1 (how are you on medicine ?)
    2 (*have-subdialogue* ((I need a refill \.)
                           ((I would like a refill of medicine \.)))) (100 :schema+args)
  1 (what is your prognosis ?)
    2 (*have-subdialogue* ((Can you tell me what kind of time we\'re looking at ?)
                           ((What is my prognosis ?)))) (100 :schema+args)

  1 (0)
    2 *reaction-to-question-off-topic* (0 :subtree)
))


(READRULES '*reaction-to-question-off-topic*
'(
  1 (where does your daughter work ?)
    2 (She works as a school nurse in the county school system\.) (0 :out)
))
