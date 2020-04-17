(MAPC 'ATTACHFEAT
'(
  (redness red)
  (respond responded react reacted)
  (made make)
  (radiation-help help better alleviate reduce comfort comfortable)
  (pain-return come back return)
))


(READRULES '*radiation-verification-input*
'(
  1 (0 POS 0)
    2 ((I think you need radiation \.) (radiation)) (0 :gist)
  1 (0 I 1 think-gen so 0)
    2 ((I think you need radiation \.) (radiation)) (0 :gist)
  1 (0 no 0)
    2 ((I do not think you need radiation \.) (radiation)) (0 :gist)
  1 (0 NEG 1 think-gen 0)
    2 ((I do not think you need radiation \.) (radiation)) (0 :gist)
  1 (0)
    2 ((NIL Gist \: nothing found for radiation \.)) (0 :gist)
))


(READRULES '*radiation-input*
'(
  1 (0 do it 1 pain-return 0)
    2 (*general-input* (did the pain return ?)) (0 :subtree+clause)

  1 (0)
    2 *general-input* (0 :subtree)
  1 (0)
    2 ((NIL Gist \: nothing found for radiation \.)) (0 :gist)
))


(READRULES '*radiation-question*
'(
  1 (0 respond 1 to 1 radiation 0)
    2 ((Did the pain respond to radiation treatment ?) (radiation-treatment)) (0 :gist)
  1 (0 do 2 radiation 5 radiation-help 0)
    2 ((Did the pain respond to radiation treatment ?) (radiation-treatment)) (0 :gist)

  1 (0 redness 0)
    2 ((Did you get any hair loss or redness during radiation treatment ?) (radiation-treatment)) (0 :gist)
  1 (0 hair loss 0)
    2 ((Did you get any hair loss or redness during radiation treatment ?) (radiation-treatment)) (0 :gist)
  1 (0 do you 3 radiation 0)
    2 ((Did you get radiation treatment ?) (radiation-treatment)) (0 :gist)
  1 (0 radiation treatment 0)
    2 ((Did you get radiation treatment ?) (radiation-treatment)) (0 :gist)
))


(READRULES '*radiation-reaction*
'(
  1 (0)
    2 (*have-subdialogue* ((What about chemotherapy?)
                           ((Do you think chemotherapy will help ?)))) (0 :schema+args)
))