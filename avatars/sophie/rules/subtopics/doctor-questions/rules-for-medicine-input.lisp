(MAPC 'ATTACHFEAT
  '(
    (med-take take taking get getting use using)
    (work working help helping treat treating effective)
    (often frequent frequently much)
    (med-time every time times hour hours minute minutes day days week weeks)
    (want need)
    (med-better better stronger effective heavier)
    (med-for for work)
    (med-help care help helping helpful better effective work working)
  ))


(READRULES '*medicine-input*
'(

  ;; ; The following needs modification
  ;; 1 (0 sure \, I can arrange that 0)
  ;;   2 ((I will give you more medicine \.) (stronger-medicine)) (0 :gist)

  ; Possibly too general, might need refining
  1 (0 med-narcotic 0)
    2 ((You should take a narcotic \.) (medicine-request)) (0 :gist)

  1 (0)
    2 *general-input* (0 :subtree)
  1 (0)
    2 ((NIL Gist \: nothing found for medicine \.)) (0 :gist)
))


(READRULES '*medicine-question*
'(
  ; Do you have allergies?
  1 (0 have 0 allergies 0)
    2 ((Do you have allergies to any medicine ?) (medicine-allergies)) (0 :gist)

  ; How often are you taking medication?
  1 (0 how 1 often 3 med-take 0)
    2 ((How often are you taking medication ?) (medicine-frequency)) (0 :gist)
  1 (0 be 0 med-take 2 often 0)
    2 ((How often are you taking medication ?) (medicine-frequency)) (0 :gist)
  1 (0 be 0 med-take 2 med-time 0)
    2 ((How often are you taking medication ?) (medicine-frequency)) (0 :gist)

  ; Does taking the medicine more frequently help?
  1 (0 medicine-taking 3 frequently 5 med-help 0)
    2 ((Does taking medication more frequently help ?) (medicine-frequency)) (0 :gist)
  1 (0 med-help 5 medicine-taking 3 frequently 0)
    2 ((Does taking medication more frequently help ?) (medicine-frequency)) (0 :gist)
  1 (0 medicine-taking 5 med-help 5 frequently 0)
    2 ((Does taking medication more frequently help ?) (medicine-frequency)) (0 :gist)

  ; Are you taking [some med]?
  1 (0 be 2 you 8 med-take 0 medicine-particular 0)
    2 ((Are you taking 8 ?) (medicine)) (0 :gist)
  1 (0 what medicine 3 you 2 med-take 0)
    2 ((What medicine are you taking ?) (medicine)) (0 :gist)

  ; Is the pain medication working?
  1 (0 be-aux 3 medicine-taking 3 med-help 3 pain 0)
    2 ((Is the pain medication working at all ?) (medicine-working)) (0 :gist)
  1 (0 be-aux 0 medicine-taking 3 do anything 0)
    2 ((Is the pain medication working at all ?) (medicine-working)) (0 :gist)
  1 (0 be-aux 0 medicine-taking 0 med-help 3 at all 0)
    2 ((Is the pain medication working at all ?) (medicine-working)) (0 :gist)
  1 (0 be-aux 0 medicine-taking 0 med-help 0)
    2 ((Is the pain medication working ?) (medicine-working)) (0 :gist)
  1 (0 how 0 medicine-taking 0 med-help 0)
    2 ((Is the pain medication working ?) (medicine-working)) (0 :gist)

  ; Do you want stronger pain medication?
  1 (0 do 1 you 3 want 3 med-better 1 medicine-taking 0)
    2 ((Do you want stronger pain medication ?) (medicine-request)) (0 :gist)

  ; Do you need more medicine?
  1 (0 do 1 you 3 want 2 medicine-taking 0)
    2 ((Do you need more medicine ?) (medicine-refill)) (0 :gist)
  1 (0 how are you 1 medicine-taking 0); e.g., how are you on medicine?
    2 ((Do you need more medicine ?) (medicine-refill)) (0 :gist)
))


(READRULES '*medicine-reaction*
'(
  ;; 1 (0 I will give you more medicine 0)
  ;;   2 (Great\, thank me !) (0 :out)
))