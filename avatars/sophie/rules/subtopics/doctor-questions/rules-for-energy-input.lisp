(MAPC 'ATTACHFEAT
'(
  (have having)
  (trouble troubles problem problems)
  (concentrate concentrating focus focusing)
))


(READRULES '*energy-input*
'(
  1 (0)
    2 *general-input* (0 :subtree)
))


(READRULES '*energy-question*
'(
  1 (0 trouble 1 concentrate 0)
    2 ((Have you had trouble concentrating ?) (energy)) (0 :gist)
  1 (0 you 1 have 1 energy 0)
    2 ((How is your energy ?) (energy)) (0 :gist)
  1 (0 trouble 1 energy 0)
    2 ((How is your energy ?) (energy)) (0 :gist)
  1 (0)
    2 ((NIL Gist)) (0 :gist)
))


(READRULES '*energy-reaction*
'(

))