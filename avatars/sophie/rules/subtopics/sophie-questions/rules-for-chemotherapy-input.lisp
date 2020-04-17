(MAPC 'ATTACHFEAT
'(
))


(READRULES '*chemotherapy-input*
'(
  ; Yeah, you’re ((gonna need it)) because radiation works only where it (( hits     )).
  ; And it doesn’t work on the whole body. But once the lung cancer has gone from the lung to
  ; the bone we know it’s in other areas and we might be able to pick up if it’s in other areas.
  ; The only way we can catch those is by ((chemotherapy)).  So, but I need to see your brain ((      )).
  ; The pain is mainly in the back, no? And the chest.

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


  1 (0 POS 0)
    2 ((I think you need chemotherapy \.) (chemotherapy)) (0 :gist)
  1 (0 I 1 think-gen so 0)
    2 ((I think you need chemotherapy \.) (chemotherapy)) (0 :gist)
  1 (0 no 0)
    2 ((I do not think you need chemotherapy \.) (chemotherapy)) (0 :gist)
  1 (0 NEG 1 think-gen 0)
    2 ((I do not think you need chemotherapy \.) (chemotherapy)) (0 :gist)

  1 (0)
    2 *general-input* (0 :subtree)
  1 (0)
    2 ((NIL Gist \: nothing found for chemotherapy \.)) (0 :gist)
))


(READRULES '*chemotherapy-question*
'(
  1 (0 did 4 tell 4 you 4 need chemotherapy 0)
    2 ((Did your doctor mention chemotherapy ?) (chemotherapy)) (0 :gist)
  1 (0 did 8 you 4 about chemotherapy 0)
    2 ((Did your doctor mention chemotherapy ?) (chemotherapy)) (0 :gist)
  1 (0 mention 3 chemotherapy 0)
    2 ((Did your doctor mention chemotherapy ?) (chemotherapy)) (0 :gist)
  1 (0 think 3 chemotherapy 0)
    2 ((What are your feelings about chemotherapy ?) (chemotherapy)) (0 :gist)
))


(READRULES '*chemotherapy-reaction*
'(
  1 (0)
    2 (*have-subdialogue* ((I hear about people getting sick and losing hair during chemotherapy\. What are some of the side effects?)
                           ((What are the side effects of chemotherapy ?)))) (0 :schema+args)
))