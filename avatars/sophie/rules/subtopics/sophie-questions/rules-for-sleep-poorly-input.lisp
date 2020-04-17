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
  (work working effective help helping)
  (spread spreading metastasized metastatic)
))


(READRULES '*sleep-poorly-input*
'(
  1 (0 medicine-gen 1 neg 1 work 0)
    2 ((You are sleeping poorly because the pain medication is no longer working \.) (sleeping-poorly)) (0 :gist)

  1 (6 side 1 effect 0)
    2 ((You are sleeping poorly because of a side effect from a medication \.) (sleeping-poorly)) (0 :gist)

  1 (0 illness 2 spread 0)
    2 ((You are sleeping poorly because the cancer has spread \.) (test-results)) (0 :gist)
  1 (0 illness 2 worse 0)
    2 ((Your cancer has gotten worse \.) (test-results)) (0 :gist)

  1 (0 medicine-gen 0)
    2 *medicine-working-input* (0 :subtree)
  1 (0 something 1 med-better 0)
    2 *medicine-working-input* (0 :subtree)

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
  1 (You are sleeping poorly because of a side effect from a medication \.)
    2 (You see\. If the medicine is going to make you tired no matter what\, you\'d rather at least have the pain controlled\.) (0 :out)
  1 (You are sleeping poorly because the cancer has spread \.)
    2 (*have-subdialogue* ((Does that mean that the cancer has gotten worse?)
                           ((Has the cancer gotten worse ?)))) (0 :schema+args)
  1 (0)
    2 (You see\.) (0 :out)
))