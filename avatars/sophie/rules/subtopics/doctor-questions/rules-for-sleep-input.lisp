(MAPC 'ATTACHFEAT
'(
  (sleep sleeping)
  (okay alright well good fine)
  (problem problems issue issues hard)
  (lot much)
  (often frequent frequently much)
  (wake waking)
  (day daytime)
  (sleep-medication ambien nyquil lunesta )
))


(READRULES '*sleep-input*
'(
  

  1 (0 sleep 2 during 3 day 0)
    2 (*sleep-question* (do you sleep during the day ?)) (0 :subtree+clause)
  1 (0 how 1 often 0 wake)
    2 (*sleep-question* (how often are you waking up at night ?)) (0 :subtree+clause)

  1 (0)
    2 *general-input* (0 :subtree)
  1 (0)
    2 ((Nil Gist \: nothing found for sleeping well \.)) (0 :gist)
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
  1 (0 how often 0)
    2 ((How often are you waking up at night ?) (waking-frequency)) (0 :gist)
  1 (0 sleep 2 during 3 day 0)
    2 ((Do you sleep during the day ?) (sleep)) (0 :gist)
))


(READRULES '*sleep-reaction*
'(

))