(MAPC 'ATTACHFEAT
'(
))


(READRULES '*treatment-choices-input*
'(
  1 (0 chemotherapy 0)
    2 ((Chemotherapy is one option \.) (chemotherapy)) (0 :gist)
  1 (0)
    2 *general-input* (0 :subtree)
))


(READRULES '*treatment-choices-question*
'(

))


(READRULES '*treatment-choices-reaction*
'(

))