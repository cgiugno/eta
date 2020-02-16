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


(READRULES '*prognosis-question*
'(
  1 (0 do you know what 2 doctor said 2 prognosis ?); e.g., do you know what your previous doctor said about your prognosis?
    2 ((What is your prognosis ?)) (0 :gist)
  1 (0)
    2 ((NIL Gist)) (0 :gist)
))


(READRULES '*prognosis-reaction*
'(
  1 (0 five years left alive 0)
    2 (Ah\, that\'s unfortunate\.) (0 :out) ; NOTE: this is where we might call another schema for
                                            ; asking a follow-up question like "how should I talk to my
                                            ; family about this?" or something.
))