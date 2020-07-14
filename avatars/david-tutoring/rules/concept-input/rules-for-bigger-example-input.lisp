;;
;; User response to system asking if they understand a concept
;; 

(MAPC 'ATTACHFEAT
  '(
    (yes yeah yup)
    (think believe)
    (bigger larger taller wider longer)
    (smaller shorter narrower)
    (example one)
    (build make create)
  ))

(READRULES '*bigger-example-input*
'(
  ; User replies with some form of 'no'
  1 (1 NEG 4)
    2 ((I do not want to make a bigger example of the concept \.)) (0 :gist)
))