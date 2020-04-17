(MAPC 'ATTACHFEAT
'(
  (number one number-plur)
  (number-plur two three four five six seven eight nine ten eleven twelve thirteen fourteen fifteen)
  (number-vague few some many several couple)
  (elapsed-time-plur years months)
  (elapsed-time year month elapsed-time-plur)
  (predict tell say know)
  (lose losing)
))


(READRULES '*prognosis-verification-input*
'(

))


(READRULES '*prognosis-input*
'(  

  ; You have life for about up to two years
  1 (0 number 2 elapsed-time 0)
    2 ((The prognosis is that you may live for 2 4 \.) (prognosis)) (0 :gist)
  1 (0 number-vague 2 elapsed-time 0)
    2 ((The prognosis is that you may live for several 4 \.) (prognosis)) (0 :gist)
  1 (0 a 2 elapsed-time 0)
    2 ((The prognosis is that you may live for a 4 \.) (prognosis)) (0 :gist)

  ; There is no cure
  1 (0 NEG 2 cure 0)
    2 ((The prognosis is that you cannot be cured \.) (prognosis)) (0 :gist)
  1 (0 NEG 8 curable 0)
    2 ((The prognosis is that you cannot be cured \.) (prognosis)) (0 :gist)
  1 (0 NEG 2 go away 0)
    2 ((The prognosis is that you cannot be cured \.) (prognosis)) (0 :gist)
  1 (0 fight 2 rest 4 life 0)
    2 ((The prognosis is that you cannot be cured \.) (prognosis)) (0 :gist)
  1 (0 lose 1 battle 0)
    2 ((The prognosis is that you cannot be cured \.) (prognosis)) (0 :gist)

  ; TODO: fix issue with extracting multiple time gist clauses from this:
  ; Good question. It’s hard to tell. If you were to read a book people would say with treatment people live about up to two years. That being said, we have patients in our practice that live much longer.  And that being said, we have patients in our practice that don’t live that long. And it really depends on the aggressiveness of your disease.  So, back in the day when we didn’t have treatments for lung cancer people would pass away within less than six months. That was the norm in an untreated lung cancer patient. And now we have patients living several years.  And the good part, if there is a silver lining in the treatment of lung cancer, is that there are so many new treatments coming out. And that my hope is that my patients currently being treated for lung
  ; cancer, these things that come out as time goes on. Because the hard part about any of these chemos is they don’t work forever.
  ; So they are smart in the sense that they can kill off cancer cells but the hard part with them is that they don’t work forever
  ; meaning the cells become clever and they find a way around the chemo.  And so when that happens, so say for example we do this
  ; four to six times and then we’re on observation, then we do another scan.  And this would be six months later. And sure enough
  ; we do the CT scan and there’s a new spot.  A new spot on the bone, a new spot on the lung. Then we repeat usually another
  ; type of chemo. So not this one. Because we’ve learned that this one, now those cancer cells figured out a way to get around.
  ; So this is when we use another type of chemo.  We use another type of chemo and then we do an observation again and that cycle
  ; keeps continuing as long as we can.  
  ;
  ;
  ; TODO: pick up on palliative care here?
  ; But in general when cancer starts in the lung and travels to other sites in the body like the bone it’s what
  ; we call Stage 4 or metastatic lung cancer, meaning that the cancer has spread from where it started to another spot.
  ; And when lung cancer does that and it spreads to other places in the body it’s unfortunately not what we would think
  ; of as a curable situation, like Dr. [Name] shared with you.  So the -- when I say that what I mean is that the
  ; treatments that we offer are unlikely to take the cancer away and have it never come back.  Okay?
  ; The goal of the. . . of the treatments is really to try to control and contain the cancer for as long as
  ; possible and in doing so we hope to have. . . to help folks live longer and live better.  Okay.  Have better
  ; control of symptoms that would be attributable to cancer and so forth.  It’s what we call palliative
  ; treatments, palliative meaning to alleviate suffering and symptoms.  
  ;

  ; Mention of chemotherapy
  1 (0 chemotherapy 0)
    2 ((The prognosis is that your cancer should be treated with chemotherapy \.) (chemotherapy)) (0 :gist)

  ; It's hard to predict
  1 (0 hard 2 to 2 predict 0)
    2 ((The prognosis is hard to predict \.) (prognosis)) (0 :gist)

  1 (0)
    2 *general-input* (0 :subtree)
  1 (0)
    2 ((NIL Gist \: nothing found for prognosis \.)) (0 :gist)
))


(READRULES '*prognosis-question*
'(
))


(READRULES '*prognosis-reaction*
'(
  ; I asked my doctor in Florida about what’s going to happen next, he just did not want to say. 
  ; He said” These things are always hard to predict.” 
  1 (The prognosis is that you may live for number-plur elapsed-time-plur \.)
    2 (That\'s all very difficult to hear\. At this point\, you just hope you can enjoy those 9 10 with family\.) (0 :out)
  1 (The prognosis is that you may live for number elapsed-time \.)
    2 (That\'s all very difficult to hear\. At this point\, you just hope you can enjoy that 9 10 with family\.) (0 :out)
  1 (The prognosis is that you may live for several elapsed-time \.)
    2 (That\'s all very difficult to hear\. At this point\, you just hope you can enjoy those 10 with family\.) (0 :out)
  1 (The prognosis is that you may live for a elapsed-time \.)
    2 (That\'s all very difficult to hear\. At this point\, you just hope you can enjoy that 10 with family\.) (0 :out)

  1 (The prognosis is that you cannot be cured \.)
    2 (*have-subdialogue* ((I feared as much\, though it\'s still pretty upsetting\. How long do you think I have?)
                           ((What is my prognosis ?)))) (100 :schema+args)
    2 (You guess you need to make peace with it \.) (0 :out)

  1 (The prognosis is that your cancer should be treated with chemotherapy \.)
    2 (*have-subdialogue* ((I want to talk about my options in a minute\, but first I just want to know how bad it really is\.
                            How long do you think I have?)
                           ((What is my prognosis ?)))) (0 :schema+args)

  1 (The prognosis is hard to predict \.)
    2 (*have-subdialogue* ((My last doctor also just said it would be hard to predict\. I think I\'m ready to hear though\.
                            Could you please just tell me what the worst case looks like?)
                           ((What is my prognosis ?)))) (100 :schema+args)

  1 (0)
    2 (That\'s all very difficult to hear\. At this point you just want to spend time with family and prepare yourself for the worst\.) (0 :out)
))