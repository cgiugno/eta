(MAPC 'ATTACHFEAT
  '(
    (med-take take taking get getting use using)
    (work working help helping treat treating effective)
    (often frequent frequently much)
    (med-time every time times hour hours minute minutes day days week weeks)
  ))


(READRULES '*medicine-request-input*
'(
  1 (0)
    2 *general-input* (0 :subtree)
  1 (0)
    2 ((NIL Gist \: nothing found for stronger pain medication \.)) (0 :gist)
))


(READRULES '*medicine-request-question*
'(
))


(READRULES '*medicine-request-reaction*
'(
  1 (0)
    2 (Okay\, you get it \.) (0 :out)
))