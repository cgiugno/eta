(MAPC 'ATTACHFEAT
  '(
    (med-take take taking get getting use using)
    (work working help helping treat treating effective)
    (often frequent frequently much)
    (med-time every time times hour hours minute minutes day days week weeks)
  ))


(READRULES '*medicine-request-input*
'(
  ;; "So my recommendation is you take a stronger medicine and that there is no ceiling on the dose of narcotic"

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
    2 (*have-subdialogue* ((Yeah\, I think I should take a stronger pain medication\. The current one isn\'t working well\.
                            What are the side effects?)
                           ((I want a stronger pain medication \.) (What are the side effects of stronger pain medication ?)))) (100 :schema+args)
    2 (You think having the stronger pain medication would help\.) (0 :out)
  1 (0)
    2 (Okay\, you get it \.) (0 :out)
))