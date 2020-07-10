(MAPC 'ATTACHFEAT
'(
  (have having)
  (trouble troubles problem problems)
  (concentrate concentrating focus focusing)
  (ment-health anxiety anxious depression depressed)
  (work working help helping treat treating effective)
  (med-take take taking get getting use using)
  (med-give recommend recommending recommended recommend advise advising advises advised suggests suggesting suggested suggest gives give given giving prescribe prescribes prescribed prescribed)
  (antidepressant SSRI Prozac fluoxetine sertraline Zoloft Celexa citalopram Lexapro escitalopram paroxetine Paxil Pexeva)
  (therapy therapist counseling counselor)
  ))


(READRULES '*energy-input*
'(
  1 (0 med-give 5 antidepressant)
    2 (I can give you an antidepressant \.) (medicine-request)) (0 :gist)
  1 (0 med-take 5 antidepressant)
    2 ((You should take an antidepressant \.) (medicine-request)) (0 :gist)
  1 (0 therapy 0)
    2 ((You should see a therapist \.) (energy) (0 :gist)
  1 (0)
    2 *general-input* (0 :subtree)
))


(READRULES '*energy-question*
'(
  1 (0 be-aux 5 ment-health 5)
    2 ((How is your mental health ?) (energy)) (0 :gist)
  1 (0 trouble 1 concentrate 0)
    2 ((Have you had trouble concentrating ?) (energy)) (0 :gist)
  1 (0 you 1 have 1 energy 0)
    2 ((How is your energy ?) (energy)) (0 :gist)
  1 (0 trouble 1 energy 0)
    2 ((How is your energy ?) (energy)) (0 :gist)
  1 (0)
    2 ((NIL Gist)) (0 :gist)
))


(READRULES '*energy-reaction*
'(
  1 (I can give you an antidepressant \.)
    2 (*have-subdialogue* ((I see\. Would this help me with my pain ?)
                           ((Will an antidepressant help with my pain ?)))) (100 :schema+args)
    2 (Okay\. You\'ll give the antidepressant medication a shot\.) (0 :out)
  1 (You should take an antidepressant \.)
    2 (*have-subdialogue* ((I see\. Would this help me with my pain ?)
                           ((Will an antidepressant help with my pain ?)))) (100 :schema+args)
    2 (Okay\. You\'ll give the antidepressant medication a shot\.) (0 :out)
  1 (You should see a therapist \.)
    2 (*have-subdialogue* ((I\'m not sure\. Do you think there\'s any medication I can try first ?)
                           ((Should I try medication before I try therapy ?)))) (100 :schema+args)
    2 (Okay\. You\'ll give the therapy a shot\.) (0 :out)
))
