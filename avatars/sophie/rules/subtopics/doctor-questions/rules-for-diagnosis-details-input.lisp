(MAPC 'ATTACHFEAT
'(
  (lead leading)
  (before prior lead)
  (know aware understand)
  (bring brings)
  (symptom symptoms)
  (symptom-weight weight skinny skinnier heavy heavier fat)
  (symptom-appetite appetite eating)
  (diagnosis-symptom symptom weight skinny skinnier appetite eating)
  (diagnosis-non-symptom headache headaches chill chills fever fevers nausea eyesight eyes)
  (lose lost)
  (much lot)
))


(READRULES '*diagnosis-details-input*
'(
  1 (0 medicine-gen 0)
    2 *medicine-working-input* (0 :subtree)
  1 (0 something 1 med-better 0)
    2 *medicine-working* (0 :subtree)

  1 (1 how much 1)
    2 (*diagnosis-details-question* (how much weight have you lost ?)) (0 :subtree+clause)
  1 (0 how much 0 lose 0)
    2 (*diagnosis-details-question* (how much weight have you lost ?)) (0 :subtree+clause)
  1 (0 do pron 2 lose 3 lot 0)
    2 (*diagnosis-details-question* (how much weight have you lost ?)) (0 :subtree+clause)
  1 (0 have pron 2 lose 3 lot 0)
    2 (*diagnosis-details-question* (how much weight have you lost ?)) (0 :subtree+clause)
  1 (0 wh_ do 2 lose 0)
    2 (*diagnosis-details-question* (how much weight have you lost ?)) (0 :subtree+clause)

  1 (0)
    2 *general-input* (0 :subtree)
  1 (0)
    2 ((NIL Gist \: nothing found for diagnosis details \.)) (0 :gist)
))


(READRULES '*diagnosis-details-question*
'(
  ; How much weight have you lost?
  1 (0 how much symptom-weight 0 lose 0)
    2 ((How much weight have you lost ?) (weight-loss)) (0 :gist)
  1 (0 do pron 2 lose 3 lot 0 symptom-weight 0)
    2 ((How much weight have you lost ?) (weight-loss)) (0 :gist)
  1 (0 have pron 2 lose 3 lot 0 symptom-weight 0)
    2 ((How much weight have you lost ?) (weight-loss)) (0 :gist)

  ; What symptoms do you have?
  1 (0 wh_ 1 symptom 0)
    2 ((What symptoms do you have ?) (symptoms)) (0 :gist)
  1 (0 do pron 3 symptom 0)
    2 ((What symptoms do you have ?) (symptoms)) (0 :gist)
  1 (0 more-info 4 symptom 0)
    2 ((What symptoms do you have ?) (symptoms)) (0 :gist)
  1 (0 have pron 5 diagnosis-symptom 0)
    2 (0 symptom-weight 0)
      3 ((Have you changed weight ?) (symptoms)) (0 :gist)
    2 (0 symptom-appetite 0)
      3 ((Have you changed appetite ?) (symptoms)) (0 :gist)

  ; Do you have [non-symptom]?
  1 (1 do 0 diagnosis-non-symptom 0)
    2 ((Do you have the symptom of 4 ?) (symptoms)) (0 :gist)
  1 (1 have 0 diagnosis-non-symptom 0)
    2 ((Do you have the symptom of 4 ?) (symptoms)) (0 :gist)
  1 (0 any diagnosis-non-symptom 0)
    2 ((Do you have the symptom of 3 ?) (symptoms)) (0 :gist)

  ; Do you understand your test results?
  1 (1 be-aux 2 you know 3 diagnosis-tests 0)
    2 ((Do you know what the tests say ?) (test-results)) (0 :gist)

  ; How did you get your diagnosis?
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