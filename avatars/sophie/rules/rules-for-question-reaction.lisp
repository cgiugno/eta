(mapc 'attachfeat
  '()
)

(READRULES '*reaction-to-question*
'(
  ;; 1 (how are you on medicine ?)
  ;;   2 (*medicine-schema* ((1 2 3 4 5 6))) (100 :schema+args)
  ;; 1 (what is your prognosis ?)
  ;;   2 (*prognosis-schema* ((1 2 3 4 5))) (100 :schema+args)

  ; General format of the below rules:
  ; 1 (pattern)
  ;   2 (*have-subdialogue* ((Answer to output \.)
  ;                          ((Gist clause one \.) (Gist clause two \.)))) (100 :schema+args)
  1 (how are you on medicine ?)
    2 (*have-subdialogue* ((I need a refill \.)
                           ((I would like a refill of medicine \.)))) (100 :schema+args)
  1 (what is your prognosis ?)
    2 (*have-subdialogue* ((Can you tell me what kind of time we\'re looking at ?)
                           ((What is my prognosis ?)))) (100 :schema+args)

))
