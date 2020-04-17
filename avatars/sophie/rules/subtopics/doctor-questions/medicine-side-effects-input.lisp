(MAPC 'ATTACHFEAT
'(
  (effect effects)

  (side-effect-fatigue fatigue fatigued exhaustion exhausted sleepiness sleepy tiredness tired energy)
  (side-effect-appetite appetite eating eat hunger hungry)
  (side-effect-nausea nausea vomiting vomit sick sickness)
  (side-effect-neuropathy neuropathy numb numbness tingle tingling)

  (side-effects-insignificant side-effect-fatigue side-effect-appetite)
  (side-effects-moderate side-effects-nausea diarrhea)
  (side-effects-significant hair side-effect-neuropathy)
))


(READRULES '*medicine-side-effects-input*
'(
  1 (0 hair 0)
    2 ((A side effect of the medication is hair loss \.) (medicine-side-effects)) (0 :gist)
  1 (0 side-effect-neuropathy 0)
    2 ((A side effect of the medication is neuropathy \.) (medicine-side-effects)) (0 :gist)
  1 (0 side-effect-nausea 0)
    2 ((A side effect of the medication is nausea \.) (medicine-side-effects)) (0 :gist)
  1 (0 diarrhea 0)
    2 ((A side effect of the medication is diarrhea \.) (medicine-side-effects)) (0 :gist)
  1 (0 side-effect-fatigue 0)
    2 ((A side effect of the medication is fatigue \.) (medicine-side-effects)) (0 :gist)
  1 (0 side-effect-appetite 0)
    2 ((A side effect of the medication is loss of appetite \.) (medicine-side-effects)) (0 :gist)
  1 (0)
    2 *general-input* (0 :subtree)
  1 (0)
    2 ((NIL Gist \: nothing found for side effects of a medication \.)) (0 :gist)
))


(READRULES '*medicine-side-effects-question*
'(
))


(READRULES '*medicine-side-effects-reaction*
'(
  1 (A side effect of the medication be 2 side-effects-significant 2 \.)
    2 (You think you should hold off for now and think about it more \.) (0 :out)
  1 (A side effect of the medication be 2 side-effects-moderate 2 \.)
    2 (You think you should try the medicine and see if you have problems with 8 9 10 \.) (0 :out)
  1 (A side effect of the medication be 2 side-effects-insignificant 2 \.)
    2 (You already have 8 9 10 \, so you think the new medicine is worth a try\.) (0 :out)
  1 (0)
    2 (You see\. Well\, you think it\'s worth giving it a try\.) (0 :out)
))