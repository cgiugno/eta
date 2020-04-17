(MAPC 'ATTACHFEAT
  '(
    (med-take take taking get getting use using)
    (work working help helping treat treating effective)
    (often frequent frequently much)
    (med-time every time times hour hours minute minutes day days week weeks)
  ))


(READRULES '*medicine-request-input*
'(
  ; You should take morphine
  1 (0 med-narcotic 0)
    2 ((You should take a narcotic \.) (medicine-request)) (0 :gist)

  ; Do you want something better / stronger pain medication
  1 (0 do 1 you 3 want 3 med-better medicine-taking 0)
    2 (*medicine-question* (do you want stronger pain medication ?)) (0 :subtree+clause)
  1 (0 do 1 want 3 med-better 0)
    2 (*medicine-question* (do you want stronger pain medication ?)) (0 :subtree+clause)

  ; You should take something stronger / better pain medication
  1 (0 you 5 med-take 3 med-better 3 medicine-gen 0)
    2 ((You should take stronger pain medication \.) (medicine-request)) (0 :gist)
  1 (0 you 5 med-take 1 something 1 med-better 0)
    2 ((You should take stronger pain medication \.) (medicine-request)) (0 :gist)
  1 (0 you 5 want 3 med-better 2 medicine-gen 0)
    2 ((You should take stronger pain medication \.) (medicine-request)) (0 :gist)
  1 (0 you 5 want 1 something 1 med-better 0)
    2 ((You should take stronger pain medication \.) (medicine-request)) (0 :gist)

  ; Maximizing your pain medication
  1 (0 med-increase 3 medicine-gen 0)
    2 ((You should take stronger pain medication \.) (medicine-request)) (0 :gist)
  1 (0 make 1 medicine-gen 2 med-better 0)
    2 ((You should take stronger pain medication \.) (medicine-request)) (0 :gist)

  ; You should take something different
  1 (0 something 2 different 0)
    2 ((You should take something different \.) (medicine-request)) (0 :gist)
  1 (0 different 2 medicine-gen 0)
    2 ((You should take something different \.) (medicine-request)) (0 :gist)

  1 (0)
    2 *general-input* (0 :subtree)
  1 (0)
    2 ((NIL Gist \: nothing found for stronger pain medication \.)) (0 :gist)
))


(READRULES '*medicine-request-question*
'(
))


(READRULES '*medicine-request-reaction*
'(
  1 (You should take 1 med-narcotic \.)
    2 (*have-subdialogue* ((Usually when I hear about narcotics\, it\'s people getting addicted to them\. Is that a possibility here?)
                           ((Can I get addicted to narcotics ?)))) (100 :schema+args)
    2 (You think having the stronger pain medication would help\.) (0 :out)
  1 (You should take stronger pain medication \.)
    2 (*have-subdialogue* ((Yeah\, a stronger pain medication would be good\. What would the side effects be?)
                           ((I want a stronger pain medication \.) (What are the side effects of stronger pain medication ?)))) (100 :schema+args)
    2 (You think having the stronger pain medication would help\.) (0 :out)
  1 (0)
    2 (Okay\, you get it \.) (0 :out)
))