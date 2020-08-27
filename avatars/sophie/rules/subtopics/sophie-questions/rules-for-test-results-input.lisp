(MAPC 'ATTACHFEAT
'(
  (body-part lung lungs bone bones chest)
  (cure curable recover recovery solution fix miracle)
  (cancer-increase increase spread)
  (metastatic metastasized metastasize)
  (scan CT cat)
  (image picture x-ray view)
  (takes records took taken recorded produces produced provides provided makes made)
))


(READRULES '*test-results-input*
'(
  ; The cancer hasn't yet spread
  1 (0 NEG 2 cancer-increase 0)
    2 ((The test results show that the cancer hasn\'t spread \.) (test-results)) (0 :gist)
  
  ; There is no cure
  1 (0 NEG 2 cure 0)
    2 ((The test results show that you cannot be cured \.) (test-results)) (0 :gist)
  1 (0 NEG 2 go away 0)
    2 ((The test results show that you cannot be cured \.) (test-results)) (0 :gist)
  1 (0 fight 2 rest 4 life 0)
    2 ((The test results show that you cannot be cured \.) (test-results)) (0 :gist)
  1 (0 lose 1 battle 0)
    2 ((The test results show that you cannot be cured \.) (test-results)) (0 :gist)

  ; The cancer has spread
  1 (0 spread 0)
    2 ((The test results show that your cancer has spread \.) (test-results)) (0 :gist)
  1 (0 illness 5 in 3 body-part)
    2 ((The test results show that your cancer has spread \.) (test-results)) (0 :gist)
  1 (0 tumor 2 in 6 chest 0)
    2 ((The test results show that your cancer has spread \.) (test-results)) (0 :gist)
  1 (0 metastatic 0)
    2 ((The test results show that your cancer has spread \.) (test-results)) (0 :gist)

  ; You have stage 4 cancer
  1 (0 stage four 0)
    2 ((The test results show that your cancer has spread \.) (test-results)) (0 :gist)
  1 (0 advanced 1 body-part 1 cancer 0)
    2 ((The test results show that your cancer has spread \.) (test-results)) (0 :gist)

  ; A CT scan is...
  1 (0 scan 2 be-aux 0)
    2 ((A CT scan is like a more accurate X ray \.) (test-results)) (0 :gist)
  1 (0 scan 3 takes 3 image 0)
    2 ((A CT scan is like a more accurate X ray \.) (test-results)) (0 :gist)
  1 (0 scan 3 image 3 takes 0)
    2 ((A CT scan is like a more accurate X ray \.) (test-results)) (0 :gist)
  1 (0 scan 5 inside 3 body 0)
    2 ((A CT scan is like a more accurate X ray \.) (test-results)) (0 :gist)

  1 (0)
    2 *general-input* (0 :subtree)
  1 (0)
    2 ((NIL Gist \: nothing found for test results \.)) (0 :gist)
))


(READRULES '*test-results-question*
'(
))


(READRULES '*test-results-reaction*
'(
  1 (The test results show that the cancer hasn\'t spread \.)
    2 (*have-subdialogue* ((My previous doctor didn\'t seem very optimistic\. So what do you think this all means for me?)
                           ((What is my prognosis ?)))) (0 :schema+args)
  1 (The test results show that you cannot be cured \.)
    2 (*have-subdialogue* ((That\'s distressing\. I was trying to prepare for the worst\, though\. What I\'m wondering
                            at this point is\, how much time do I have left?)
                           ((What is my prognosis ?)))) (0 :schema+args)
  1 (The test results show that your cancer has spread \.)
    2 (*have-subdialogue* ((I see\. That sounds pretty bleak\. What does it mean for me?)
                           ((What is my prognosis ?)))) (0 :schema+args)
  1 (0)
    2 (Oh\, you see\.) (0 :out)
))
