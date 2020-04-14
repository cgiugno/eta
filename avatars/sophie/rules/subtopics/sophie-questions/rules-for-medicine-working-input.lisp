(MAPC 'ATTACHFEAT
  '(
    (med-take take taking get getting use using)
    (work working help helping treat treating effective)
    (often frequent frequently much)
    (med-time every time times hour hours minute minutes day days week weeks)
  ))


(READRULES '*medicine-working-input*
'(
  1 (0)
    2 *general-input* (0 :subtree)
  1 (0)
    2 ((NIL Gist \: nothing found for knowing if pain medication working \.)) (0 :gist)
))


(READRULES '*medicine-working-question*
'(
))


(READRULES '*medicine-working-reaction*
'(
  1 (0)
    2 (You think for now I\'ll wait to see if the Lortab starts helping more\.) (0 :out)
))