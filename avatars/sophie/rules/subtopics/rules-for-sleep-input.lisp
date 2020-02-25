(MAPC 'ATTACHFEAT
'(
  (sleep sleeping)
  (okay alright well good fine)
  (problem problems issue issues hard)
  (lot much)
))


(READRULES '*sleep-input*
'(
  1 (0)
    2 *general-input* (0 :subtree)
))


(READRULES '*sleep-question*
'(
  1 (0 be you 2 sleep 1 okay 0)
    2 ((Have you been sleeping okay ?) (sleep)) (0 :gist)
  1 (0 have you 2 sleep 1 okay 0)
    2 ((Have you been sleeping okay ?) (sleep)) (0 :gist)
  1 (0 you 3 problem sleep 0)
    2 ((Have you been sleeping okay ?) (sleep)) (0 :gist)
  1 (0 you 2 sleep 1 lot 0)
    2 ((Have you been sleeping okay ?) (sleep)) (0 :gist)
))


(READRULES '*sleep-reaction*
'(

))