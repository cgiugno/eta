(MAPC 'ATTACHFEAT
'(
  (quit quitting stop stopping stopped forgo forgoing)
  (smoke smokes smoking smoked cigarettes cigarette tobacco)
  (taken take used use smoke)
  (before prior lead)
  (know aware understand)
  (bring brings)
  (symptom symptoms)
  (lose lost)
  (much lot)
  (often frequently much)
  (drink drinking drank wine alcohol beer drunk inebriated)
  (congratulations congrats)
  (ment-health depression mental anxiety depressed anxious schizophrenia)
  (family parents grandparents siblings)
  (history background past experience)
  (experienced experiencing having had have suffered suffering diagnosed)
  (parent parents mother father mom dad)
  (die died dead pass passed depart perish)
  (recreational drugs marijuana weed)
  ))


(READRULES '*medical-history-input*
'(
 
 ;What is your history with drinking?
 1 (0 how 1 often 5 drink 0)
    2 ((What is your history with alcohol ?) (medical-history)) (0 :gist)
  1 (0 do 1 pron 5 drink 0)
    2 ((What is your history with alcohol ?) (medical-history)) (0 :gist)
  1 (0 be-aux 1 pron 5 drink 0)
    2 ((What is your history with alcohol ?) (medical-history)) (0 :gist)
  
  ;What is your history with smoking?
  1 (0 be-aux 1 pron 5 smoke 0)
    2 ((What is your history with smoking ?) (medical-history)) (0 :gist)
  1 (0 do 1 pron 5 smoke 0)
    2 ((What is your history with smoking ?) (medical-history)) (0 :gist)
  1 (0 how 1 often 5 smoke 0)
    2 ((What is your history with smoking ?) (medical-history)) (0 :gist)
  
  ;What is your family's history with mental health ?
  1 (0 be-aux 1 family 5 history 5 ment-health 0)
    2 ((Does your family have a history of mental illness ?) (medical-history)) (0 :gist)
  1 (0 have 8 family 3 experienced 5 ment-health 0)
    2 ((Does your family have a history of mental illness ?) (medical-history)) (0 :gist)
  1 (0 history 3 ment-health 0)
    2 ((Does your family have a history of mental illness ?) (medical-history)) (0 :gist)
  1 (0 family 1 be-aux 5 history 5 ment-health 0)
    2 ((Does your family have a history of mental illness ?) (medical-history)) (0 :gist)
  
  ; Are your parents still alive?
  1 (0 be-aux 1 parent 3 alive 0)
    2 ((How did your parents die ?) (medical-history)) (0 :gist)
  1 (0 be-aux 1 parent 3 die 0)
    2 ((How did your parents die ?) (medical-history)) (0 :gist)
  1 (0 wh_ 2 be-aux 3 parent 3 die 0)
    2 ((How did your parents die ?) (medical-history)) (0 :gist)
  
  ;Have you ever taken any other drugs?
  1 (0 be-aux 1 pron 3 recreational 0)
    2 ((Have you ever taken any other drugs ?) (medical-history)) (0 :gist)
  1 (0 pron 1 be-aux 3 recreational 0)
    2 ((Have you ever taken any other drugs ?) (medical-history)) (0 :gist)
  1 (0 pron 3 history 3 recreational 0)
    2 ((Have you ever taken any other drugs ?) (medical-history)) (0 :gist)

  ;Questions related to symptoms
  1 (0 diagnosis-symptom 0)
    2 *diagnosis-details-input* (0 :subtree)
  1 (0 diagnosis-non-symptom 0)
   2 *diagnosis-details-input* (0 :subtree)
  1 (0 diagnosis-tests 0)
    2 *diagnosis-details-input* (0 :subtree)
    
  1 (0)
    2 *general-input* (0 :subtree)
  1 (0)
    2 ((NIL Gist \: nothing found for medical history \.)) (0 :gist)
))


(READRULES '*medical-history-question*
'(
  ;How often do you drink?
  1 (0 how 1 often 5 drink 0)
    2 ((What is your history with alcohol ?) (medical-history)) (0 :gist)
  1 (0 do 1 pron 5 drink 0)
    2 ((What is your history with alcohol ?) (medical-history)) (0 :gist)
  1 (0 be-aux 1 pron 5 drink 0)
    2 ((What is your history with alcohol ?) (medical-history)) (0 :gist)
   
   ;How often do you smoke?
  1 (0 how 1 often 5 smoke 0)
    2 ((What is your history with smoking ?) (medical-history)) (0 :gist)
  1 (0 do 1 pron 5 smoke 0)
    2 ((What is your history with smoking ?) (medical-history)) (0 :gist)
  1 (0 be-aux 1 pron 5 smoke 0)
    2 ((What is your history with smoking ?) (medical-history)) (0 :gist)
  
  ;What is your family's history with mental health ?
  1 (0 be-aux 1 family 5 history 5 ment-health 0)
    2 ((Does your family have a history of mental illness ?) (medical-history)) (0 :gist)
  1 (0 have 8 family 3 experienced 5 ment-health 0)
    2 ((Does your family have a history of mental illness ?) (medical-history)) (0 :gist)
  1 (0 history 3 ment-health 0)
    2 ((Does your family have a history of mental illness ?) (medical-history)) (0 :gist)
  1 (0 family 1 be-aux 5 history 5 ment-health 0)
    2 ((Does your family have a history of mental illness ?) (medical-history)) (0 :gist)
    
  ; Are your parents still alive?
  1 (0 be-aux 1 parent 3 alive 0)
    2 ((How did your parents die ?) (medical-history)) (0 :gist)
  1 (0 be-aux 1 parent 3 die 0)
    2 ((How did your parents die ?) (medical-history)) (0 :gist)
  1 (0 wh_ 2 be-aux 3 parent 3 die 0)
    2 ((How did your parents die ?) (medical-history)) (0 :gist)
   
  ;Have you ever taken any other drugs?
  1 (0 be-aux 1 pron 3 recreational 0)
    2 ((Have you ever taken any other drugs ?) (medical-history)) (0 :gist)
  1 (0 pron 1 be-aux 3 recreational 0)
    2 ((Have you ever taken any other drugs ?) (medical-history)) (0 :gist)
  1 (0 pron 3 history 3 recreational 0)
    2 ((Have you ever taken any other drugs ?) (medical-history)) (0 :gist)

  ))


(READRULES '*medical-history-reaction*
'(
  1 (0 congratulations 0)
    2 (Well\, truth be told\, you simply lost your taste for it\. But\, you appreciate my congratulations all the same\.) (0 :out)
    
))
