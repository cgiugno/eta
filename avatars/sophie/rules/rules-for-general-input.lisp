(MAPC 'ATTACHFEAT
  '(
  ))


(READRULES '*general-input*
'(
  1 (0)
    2 (NIL Gist) (0 :gist)
))


(READRULES '*general-question*
'(
  1 (0 medicine 0)
    2 *medicine-question* (0 :subtree)
  1 (0 prognosis 0)
    2 *prognosis-question* (0 :subtree)
  1 (0)
    2 (NIL Gist) (0 :gist)
))


(READRULES '*medicine-question*
'(
  1 (0 how are you on medicine ?)
    2 ((How are you on medicine ?) (medicine)) (0 :gist)
  1 (0)
    2 ((NIL Gist) (medicine)) (0 :gist)
))


(READRULES '*prognosis-question*
'(
  1 (0 prognosis 0)
    2 ((Let\'s talk about the prognosis \.) (prognosis)) (0 :gist)
  1 (0)
    2 ((NIL Gist) (prognosis)) (0 :gist)
))


(READRULES '*reaction-to-general-input*
'(
  1 (0)
    2 (It is very nice to meet me \. You hope we have many great interactions in the future\.) (100 :out)
))
