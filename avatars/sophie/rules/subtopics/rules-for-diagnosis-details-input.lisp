(MAPC 'ATTACHFEAT
'(
  (lead leading)
  (before prior lead)
  (know aware)
  (bring brings)
))


(READRULES '*diagnosis-details-input*
'(
  1 (0)
    2 *general-input* (0 :subtree)
))


(READRULES '*diagnosis-details-question*
'(
  1 (0 what 2 bring you 0)
      2 ((How did you get your diagnosis ?) (diagnosis-details)) (0 :gist)
  1 (0 what 1 before 1 to 2 diagnosis 0)
    2 ((How did you get your diagnosis ?) (diagnosis-details)) (0 :gist)
  1 (0 what 2 before 0 diagnosis 0)
    2 ((How did you get your diagnosis ?) (diagnosis-details)) (0 :gist)
  1 (0 how do 3 know 0 diagnosis 0)
    2 ((How did you get your diagnosis ?) (diagnosis-details)) (0 :gist)
  1 (0 how 0 find out 0 diagnosis 0)
    2 ((How did you get your diagnosis ?) (diagnosis-details)) (0 :gist)
))


(READRULES '*diagnosis-details-reaction*
'(

))