(MAPC 'ATTACHFEAT
'(
  (redness red)
))


(READRULES '*radiation-input*
'(
  1 (0 hair loss 0)
    2 (*radiation-question* (did you get any hair loss or redness during radiation treatment ?)) (0 :subtree+clause)
  1 (0 redness 0)
    2 (*radiation-question* (did you get any hair loss or redness during radiation treatment ?)) (0 :subtree+clause)
  1 (0)
    2 *general-input* (0 :subtree)
))


(READRULES '*radiation-question*
'(
  1 (0 hair loss 2 redness 0)
    2 ((Did you get any hair loss or redness during radiation treatment ?) (radiation-treatment)) (0 :gist)
  1 (0 do you 3 radiation 0)
    2 ((Did you get radiation treatment ?) (radiation-treatment)) (0 :gist)
  1 (0 radiation treatment 0)
    2 ((Did you get radiation treatment ?) (radiation-treatment)) (0 :gist)
))


(READRULES '*radiation-reaction*
'(

))