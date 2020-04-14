(MAPC 'ATTACHFEAT
'(
  (sleep sleeping)
  (okay alright well good fine)
  (problem problems issue issues hard)
  (lot much)
  (often frequent frequently much)
  (wake waking)
  (day daytime)
  (sleep-medication ambien nyquil lunesta)
))


(READRULES '*sleep-poorly-input*
'(
  

  1 (0 sleep 2 during 3 day 0)
    2 (*sleep-question* (do you sleep during the day ?)) (0 :subtree+clause)
  1 (0 how 1 often 0 wake)
    2 (*sleep-question* (how often are you waking up at night ?)) (0 :subtree+clause)

  1 (0)
    2 *general-input* (0 :subtree)
  1 (0)
    2 ((Nil Gist \: nothing found for why I am sleeping poorly \.)) (0 :gist)
))


(READRULES '*sleep-poorly-question*
'(
))


(READRULES '*sleep-poorly-reaction*
'(
  1 (0)
    2 (You see\.) (0 :out)
))