(MAPC 'ATTACHFEAT
  '(
  ))


(READRULES '*general-input*
'(
  1 (0 nice to meet you 0)
    2 ((It is nice to meet you \.)) (0 :gist)
  1 (0 medicine 0)
    2 *medicine-question* (0 :subtree)
  1 (0 prognosis 0)
    2 *prognosis-question* (0 :subtree)
  1 (0)
    2 ((NIL Gist)) (0 :gist)
))


(READRULES '*medicine-question*
'(
  1 (0 how are you 1 medicine ?); e.g., how are you on medicine?
    2 ((How are you on medicine ?)) (0 :gist)
  1 (0)
    2 ((NIL Gist)) (0 :gist)
))


(READRULES '*prognosis-question*
'(
  1 (0 do you know what 2 doctor said 2 prognosis ?); e.g., do you know what your previous doctor said about your prognosis?
    2 ((What is your prognosis ?)) (0 :gist)
  1 (0)
    2 ((NIL Gist)) (0 :gist)
))


(READRULES '*reaction-to-general-input*
'(
  1 (0)
    2 (It is very nice to meet me \. You hope we have many great interactions in the future\.) (100 :out)
))
