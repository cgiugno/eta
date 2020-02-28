(MAPC 'ATTACHFEAT
'(
  (drink drank drunk drinks drinking)
  (drink drinks beer alcohol liquor booze)
  (drug drugs)
))


(READRULES '*substance-input*
'(
  1 (0)
    2 *general-input* (0 :subtree)
))


(READRULES '*substance-question*
'(
  1 (0 you 2 drink 1 alcohol 0)
    2 ((Do you drink alcohol ?) (substance)) (0 :gist)
  1 (how often 3 you 2 drink 0)
    2 ((How often do you drink ?) (substance)) (0 :gist)
))


(READRULES '*substance-reaction*
'(

))