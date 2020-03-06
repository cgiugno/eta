(MAPC 'ATTACHFEAT
'(
 (lose lost)
 (gain gained)
))


(READRULES '*weight-input*
'(
    1 (0)
        2 *general-input* (0 :subtree)
))


(READRULES '*weight-question*
'(
    1 (0 have you 2 lose 1 weight 0)
        2 ((Have you lost weight ?) (weight)) (0 :gist)
    1 (0 be you 2 lose 1 weight 0)
        2 ((Have you lost weight ?) (weight)) (0 :gist)
))


(READRULES '*weight-reaction*
'(

))