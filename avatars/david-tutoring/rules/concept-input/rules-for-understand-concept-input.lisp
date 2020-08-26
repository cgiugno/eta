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

(READRULES '*understand-concept-input*
'(
  ; User asks to make a bigger example
  1 (0 bigger 0)
    2 ((I want to make a bigger example of the concept \.)) (0 :gist)
  1 (0 build 2 another 1 example 0)
    2 ((I want to make a bigger example of the concept \.)) (0 :gist)

  ; User asks to make a smaller example
  1 (0 smaller 0)
    2 ((I want to make a smaller example of the concept \.)) (0 :gist)

  ; User replies with some form of 'yes'
  1 (1 yes 5)
    2 ((I understand the concept \.)) (0 :gist)
  1 (1 I think 2)
    2 ((I understand the concept \.)) (0 :gist)
  1 (1 I do 2)
    2 ((I understand the concept \.)) (0 :gist)
  
  ; User replies with some form of 'no'
  1 (1 NEG 0)
    2 ((I do not understand the concept \.))
))