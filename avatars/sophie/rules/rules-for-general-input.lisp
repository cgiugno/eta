(MAPC 'ATTACHFEAT
'(
  (meet meeting)
  (come came)
  (anyone-here-with-you with alone by)
))


(READRULES '*general-input*
'(
  ; Generic greeting
  1 (0 nice to meet you 0)
    2 ((It is nice to meet you \.)) (0 :gist)
  ; If asked if anyone is here with you
  1 (0 here anyone-here-with-you 0)
    2 *anyone-here-with-you-question* (0 :subtree)
  1 (0 come anyone-here-with-you 0)
    2 *anyone-here-with-you-question* (0 :subtree)

  ; The following two will need modification
  1 (0 medicine 0)
    2 *medicine-question* (0 :subtree)
  1 (0 prognosis 0)
    2 *prognosis-question* (0 :subtree)
    
  1 (0)
    2 ((NIL Gist)) (0 :gist)
))


(READRULES '*general-reaction*
'(
  1 (0 nice 1 meet you 0)
    2 (Thank me \.) (100 :out)
  ; If no gist clauses were extracted from user, ask them to repeat the question (one of a few possible phrasings)
  1 (0)
    2 (0)
      3 (*have-subdialogue* ((I\'m sorry\, I didn\'t quite understand\. Can you say it again ?) nil)) (4 :schema+args)
    2 (0)
      3 (*have-subdialogue* ((Would you mind repeating ?) nil)) (4 :schema+args)
    2 (0)
      3 (*have-subdialogue* ((Sorry\, I didn\'t catch that\. Could you repeat it ?) nil)) (4 :schema+args)
))
