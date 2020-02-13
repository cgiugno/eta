(MAPC 'ATTACHFEAT
  '(
  ))


(READRULES '*medicine-input*
'(
  1 (0 sure \, I can arrange that 0)
    2 ((I will give you more medicine \.) (medicine)) (0 :gist)
  1 (0)
    2 *general-input* (0 :subtree)
))


(READRULES '*medicine-reaction*
'(
  1 (0 I will give you more medicine 0)
    2 (Great\, thank me !) (0 :out)
))