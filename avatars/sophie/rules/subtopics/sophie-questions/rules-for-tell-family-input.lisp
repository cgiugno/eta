(MAPC 'ATTACHFEAT
'(
  (family relative relatives children daughter son child grandchildren sibling siblings)
  (told mentioned explained said)
  (know knows idea realize knowledge aware informed inform)
  (cancer condition illness sickness)
  (anyone any someone person)
  (conversation talk discussion appointment)
  (want like desire)
))


(READRULES '*tell-family-input*
'(
  ; Does your family know that you have cancer ?
  1 (0 does 4 family 4 know 0)
    2 ((Does your family know that you have cancer ?) (tell-family)) (0 :gist)
  1 (0 be-aux 4 family 4 know 0)
    2 ((Does your family know that you have cancer ?) (tell-family)) (0 :gist)
  1 (0 be-aux 3 cancer 3 told 5 family 0)
    2 ((Does your family know that you have cancer ?) (tell-family)) (0 :gist)

  ; Is there anyone else you want here for this conversation?
  1 (0 anyone 5 here 4 want 5 conversation 0)
    2 ((Is there someone you want here for this conversation ?) (tell-family)) (0 :gist)
  1 (0 want 3 anyone 7 conversation 0)
    2 ((Is there someone you want here for this conversation ?) (tell-family)) (0 :gist)
  1 (0 anyone 5 want 4 here 5 conversation 0)
    2 ((Is there someone you want here for this conversation ?) (tell-family)) (0 :gist)

  1 (0)
    2 *general-input* (0 :subtree)
  1 (0)
    2 ((NIL Gist \: nothing found for tell family \.)) (0 :gist)
))


(READRULES '*tell-family-question*
'(
  ; Does your family know that you have cancer ?
  1 (0 does 4 family 4 know 0)
    2 ((Does your family know that you have cancer ?) (tell-family)) (0 :gist)
  1 (0 be-aux 4 family 4 know 0)
    2 ((Does your family know that you have cancer ?) (tell-family)) (0 :gist)
  1 (0 be-aux 3 cancer 3 told 5 family 0)
    2 ((Does your family know that you have cancer ?) (tell-family)) (0 :gist)

 ; Is there anyone else you want here for this conversation?
  1 (0 anyone 5 here 4 want 5 conversation 0)
    2 ((Is there someone you want here for this conversation ?) (tell-family)) (0 :gist)
  1 (0 want 3 anyone 7 conversation 0)
    2 ((Is there someone you want here for this conversation ?) (tell-family)) (0 :gist)
  1 (0 anyone 5 want 4 here 5 conversation 0)
    2 ((Is there someone you want here for this conversation ?) (tell-family)) (0 :gist)

))


(READRULES '*tell-family-reaction*
'(
  1 (0)
    2 (Okay\. It will be difficult\, but your family and you will have to be strong\.) (0 :out)
))
