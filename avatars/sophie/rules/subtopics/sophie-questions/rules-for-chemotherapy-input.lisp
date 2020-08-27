(MAPC 'ATTACHFEAT
'(
 (palliative comfort)
 (think feel)
 (thinking feeling)
 (thoughts feelings views opinions impressions opinion impression)
))


(READRULES '*chemotherapy-input*
'(
  ; TODO: see if any of this should be incorporated
  ; The worst thing that can happen – that’s a very good question – the worst thing that could happen is two things.
  ; One is that either it doesn’t work at all which means you tried all that for nothing. Or that despite it working
  ; it will be too toxic for you, meaning you can get so sick that you’ll say I don't know that this is worth it.
  ; Those would be the worst case scenarios. The majority of the times we fall right in the middle where we get some
  ; response and yes, we get some side effects but not that terrible, not unmanageable. And when I say sick it would be
  ; not feeling good, feeling more tired. Some people do have a lot of nausea issues. That’s not very common because we
  ; have much better drugs now than we did 15, 20 years ago. So what most people have in their brains about chemotherapy
  ; and people who are just hugging the toilet, not the case anymore. It’s mostly just getting tired, really tired to
  ; the point that some people say if this is how I'm going to live I don't want that. So that’s what I can think as worse
  ; case scenarios for you.  And like I said, those would be the minority of the cases. I am not one to push but my devil’s
  ; advocate argument is always well we won’t know until we try. We can always try one or two and if it doesn't work say thank
  ; you, I gave it my best shot. But at least you can look back and say I tried.

  1 (0 palliative care 0)
    2 *comfort-care-input* (0 :subtree)
  1 (0 hospice 0)
    2 *comfort-care-input* (0 :subtree)
    
  1 (0 POS 0)
    2 ((I think you need chemotherapy \.) (chemotherapy)) (0 :gist)
  1 (0 I 1 think-gen so 0)
    2 ((I think you need chemotherapy \.) (chemotherapy)) (0 :gist)
  1 (0 no 0)
    2 ((I do not think you need chemotherapy \.) (chemotherapy)) (0 :gist)
  1 (0 NEG 1 think-gen 0)
    2 ((I do not think you need chemotherapy \.) (chemotherapy)) (0 :gist)

  1 (0 radiation 0)
    2 ((Radiation is a treatment option \.) (radiation)) (0 :gist)
  1 (0 surgery 0)
    2 ((Surgery is a treatment option \.) (surgery)) (0 :gist)

  1 (0 quality 2 life 0)
    2 ((Maintaining good quality of life is a treatment option \.) (comfort-care)) (0 :gist)
  1 (0 keep 2 comfortable 0)
    2 ((Maintaining good quality of life is a treatment option \.) (comfort-care)) (0 :gist)

  1 (0)
    2 *general-input* (0 :subtree)
  1 (0)
    2 ((NIL Gist \: nothing found for chemotherapy \.)) (0 :gist)
))


(READRULES '*chemotherapy-question*
'(
  1 (0 did 4 tell 4 you 4 need med-chemotherapy 0)
    2 ((Did your doctor mention chemotherapy ?) (chemotherapy)) (0 :gist)
  1 (0 did 8 you 4 about med-chemotherapy 0)
    2 ((Did your doctor mention chemotherapy ?) (chemotherapy)) (0 :gist)
  1 (0 mention 3 med-chemotherapy 0)
    2 ((Did your doctor mention chemotherapy ?) (chemotherapy)) (0 :gist)
  1 (0 think 3 med-chemotherapy 0)
    2 ((What are your feelings about chemotherapy ?) (chemotherapy)) (0 :gist)
  1 (0 thinking 3 med-chemotherapy)
    2 ((What are your feelings about chemotherapy ?) (chemotherapy)) (0 :gist)
  1 (0 wh_ 1 be-aux 3 thoughts 3 med-chemotherapy 0)
    2 ((What are your feelings about chemotherapy ?) (chemotherapy)) (0 :gist)
))


(READRULES '*chemotherapy-reaction*
'(
  1 (0)
    2 (*have-subdialogue* ((I hear about people getting sick and losing hair during chemotherapy\. What are some of the side effects?)
                           ((What are the side effects of chemotherapy ?)))) (100 :schema+args)
    2 (*have-subdialogue* ((How does chemotherapy usually work?)
                           ((How does chemotherapy work ?)))) (0 :schema+args)            
))
