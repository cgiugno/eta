(MAPC 'ATTACHFEAT
'(
))


(READRULES '*treatment-option-input*
'(
  1 (0 chemotherapy 0)
    2 ((Chemotherapy is a treatment option \.) (chemotherapy)) (0 :gist)
  1 (0)
    2 *general-input* (0 :subtree)
  1 (0)
    2 ((NIL Gist \: nothing found for treatment option \.)) (0 :gist)
))


(READRULES '*treatment-option-question*
'(

))


(READRULES '*treatment-option-reaction*
'(
  1 (0)
    2 (You will have to think about what you said more\.) (0 :out)
))