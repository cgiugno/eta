(MAPC 'ATTACHFEAT
  '(
  ))


(READRULES '*prognosis-input*
'(
  1 (0 five years 0)
    2 ((I think you have five years left alive \.) (prognosis)) (0 :gist)
  1 (0)
    2 *general-input* (0 :subtree)
))


(READRULES '*prognosis-reaction*
'(
  1 (0 five years left alive 0)
    2 (Ah\, that\'s unfortunate\.) (0 :out) ; NOTE: this is where we might call another schema for
                                            ; asking a follow-up question like "how should I talk to my
                                            ; family about this?" or something.
))