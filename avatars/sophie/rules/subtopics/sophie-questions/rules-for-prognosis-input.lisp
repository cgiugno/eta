(MAPC 'ATTACHFEAT
'(
))


(READRULES '*prognosis-input*
'(
  ; Response to "what does that mean for me?"


  ; Good question. It’s hard to tell. If you were to read a book people would say with treatment people live about up to two years.
  ; That being said, we have patients in our practice that live much longer.  And that being said, we have patients in our practice
  ; that don’t live that long. And it really depends on the aggressiveness of your disease.  So, back in the day when we didn’t
  ; have treatments for lung cancer people would pass away within less than six months. That was the norm in an untreated lung cancer
  ; patient. And now we have patients living several years.  And the good part, if there is a silver lining in the treatment of lung
  ; cancer, is that there are so many new treatments coming out. And that my hope is that my patients currently being treated for lung
  ; cancer, these things that come out as time goes on. Because the hard part about any of these chemos is they don’t work forever.
  ; So they are smart in the sense that they can kill off cancer cells but the hard part with them is that they don’t work forever
  ; meaning the cells become clever and they find a way around the chemo.  And so when that happens, so say for example we do this
  ; four to six times and then we’re on observation, then we do another scan.  And this would be six months later. And sure enough
  ; we do the CT scan and there’s a new spot.  A new spot on the bone, a new spot on the lung. Then we repeat usually another
  ; type of chemo. So not this one. Because we’ve learned that this one, now those cancer cells figured out a way to get around.
  ; So this is when we use another type of chemo.  We use another type of chemo and then we do an observation again and that cycle
  ; keeps continuing as long as we can.  


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
  1 (0)
    2 (That\'s very difficult to hear\. You need to mentally prepare for the worst\.) (0 :out)
))