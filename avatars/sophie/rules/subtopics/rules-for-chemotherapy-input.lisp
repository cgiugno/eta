(MAPC 'ATTACHFEAT
'(
  (chemotherapy-thoughts think thoughts thought feel feelings feeling believe)
  (mention say)
  (chemotherapy chemo)
))


(READRULES '*chemotherapy-input*
'(
  1 (0 POS 0)
    2 ((I think you need chemotherapy \.) (chemotherapy)) (0 :gist)
  1 (0 I 1 chemotherapy-thoughts so 0)
    2 ((I think you need chemotherapy \.) (chemotherapy)) (0 :gist)
  1 (0 no 0)
    2 ((I do not think you need chemotherapy \.) (chemotherapy)) (0 :gist)
  1 (0 NEG 1 chemotherapy-thoughts 0)
    2 ((I do not think you need chemotherapy \.) (chemotherapy)) (0 :gist)
  1 (0)
    2 *general-input* (0 :subtree)
))


(READRULES '*chemotherapy-question*
'(
  1 (0 did 8 you 4 about chemotherapy 0)
    2 ((Did your doctor mention chemotherapy ?) (chemotherapy)) (0 :gist)
  1 (0 mention 3 chemotherapy 0)
    2 ((Did your doctor mention chemotherapy ?) (chemotherapy)) (0 :gist)
  1 (0 think 3 chemotherapy 0)
    2 ((What are your feelings about chemotherapy ?) (chemotherapy)) (0 :gist)
))


(READRULES '*chemotherapy-reaction*
'(

))