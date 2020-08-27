(MAPC 'ATTACHFEAT
'(
  (med-chemotherapy chemotherapy chemo alkylating antitumor antimetabolites antimetabolite)
  (palliative comfort)
  (priorities priority preference preferences)
  (depend depends conditional relies hinges)
  (keep remain) 
))


(READRULES '*treatment-option-input*
'(
  1 (0 palliative care 0)
    2 ((Comfort care is a treatment option \.) (comfort-care)) (0 :gist)
  1 (0 hospice 0)
    2 ((Comfort care is a treatment option \.) (comfort-care)) (0 :gist)

  1 (0 med-chemotherapy 0)
    2 ((Chemotherapy is a treatment option \.) (chemotherapy)) (0 :gist)
  1 (0 chemotherapy 0)
    2 ((Chemotherapy is a treatment option \.) (chemotherapy)) (0 :gist)

  1 (0 radiation 0)
    2 ((Radiation is a treatment option \.) (radiation)) (0 :gist)
  1 (0 surgery 0)
    2 ((Surgery is a treatment option \.) (surgery)) (0 :gist)

  1 (0 quality 2 life 0)
    2 ((Maintaining good quality of life is a treatment option \.) (comfort-care)) (0 :gist)
  1 (0 keep 2 comfortable 0)
    2 ((Maintaining good quality of life is a treatment option \.) (comfort-care)) (0 :gist)

  1 (0 wh_ 3 be-aux 3 pron 3 priorities 0) 
    2 ((What are your priorities for your care ?) (comfort-care)) (0 :gist)
  1 (0 depend 4 priorities 0)
    2 ((What are your priorities for your care ?) (comfort-care)) (0 :gist)
  1 (0 tell 5 priorities 0)
    2 ((What are your priorities for your care ?) (comfort-care)) (0 :gist)
  1 (0 let 1 me 1 know 5 priorities 0)
    2 ((What are your priorities for your care ?) (comfort-care)) (0 :gist)


  1 (0)
    2 *general-input* (0 :subtree)
  1 (0)
    2 ((NIL Gist \: nothing found for treatment option \.)) (0 :gist)
))


(READRULES '*treatment-option-question*
'(

  1 (0 wh_ 3 be-aux 3 pron 3 priorities 0) 
    2 ((What are your priorities for your care ?) (comfort-care)) (0 :gist)
  1 (0 depend 4 priorities 0)
    2 ((What are your priorities for your care ?) (comfort-care)) (0 :gist)
  1 (0 tell 5 priorities 0)
    2 ((What are your priorities for your care ?) (comfort-care)) (0 :gist)
  1 (0 let 1 me 1 know 5 priorities 0)
    2 ((What are your priorities for your care ?) (comfort-care)) (0 :gist)
))


(READRULES '*treatment-option-reaction*
'(
  1 (Maintaining good quality of life is a treatment option \.)
    2 (*have-subdialogue* ((Keeping a high quality of life for the rest of my time is the most important thing to me right now\.
                            What\'s the best option for making sure this is possible?)
                           ((What are my options for treatment ?)))) (100 :schema+args)
  
  1 (Radiation is a treatment option \.)
    2 (*have-subdialogue* ((Do you think radiation is really going to help at this point?)
                           ((Do you think radiation will help ?)))) (0 :schema+args)

  1 (Chemotherapy is a treatment option \.)
    2 (*have-subdialogue* ((What happens if I don\'t do chemotherapy?)
                           ((What happens if I do not do chemotherapy ?)))) (100 :schema+args)
    2 (*have-subdialogue* ((Do you think chemotherapy is really going to help?)
                           ((Do you think chemotherapy will help ?)))) (0 :schema+args)
    
  1 (Comfort care is a treatment option \.)
    2 (*have-subdialogue* ((Comfort care sounds good to me\. What I want is to try to get my life back to normal as much as possible\. You know\,
                            spend time with my family\. What do I have to do for that?)
                           ((How does comfort care work ?)))) (0 :schema+args)
  1 (0)
    2 (You will have to think about what you said more\.) (0 :out)
))
