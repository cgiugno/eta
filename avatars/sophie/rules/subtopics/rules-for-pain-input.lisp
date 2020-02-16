(MAPC 'ATTACHFEAT
'(
  (pain-info learn hear tell say more additional elaborate)
  (elaborate continue describe)
  (can could)
  (scale rate rating)
  (part parts section sections area areas)
  (pain-bad bad much strongly intensely badly)
))


(READRULES '*pain-input*
'(
  ; If doctor inquires for more information
  1 (0 pain-info 2 pain-info 0)
    2 (*pain-question* (can you tell me about your pain ?)) (0 :subtree+clause)
  1 (0 go on 0)
    2 (*pain-question* (can you tell me about your pain ?)) (0 :subtree+clause)
  1 (0 elaborate 0)
    2 (*pain-question* (can you tell me about your pain ?)) (0 :subtree+clause)
  ; If doctor specifically wants you to rate your pain
  1 (0 how pain-bad 2 pain 0)
    2 (*pain-question* (how do you rate your pain ?)) (0 :subtree+clause)
  1 (0 scale 0)
    2 (*pain-question* (how do you rate your pain ?)) (0 :subtree+clause)
  1 (0)
    2 *general-input* (0 :subtree)
))


(READRULES '*pain-question*
'(
  ; How do you rate your pain?
  1 (0 scale 0)
    2 ((How do you rate your pain ?) (pain-description)) (0 :gist)
  1 (0 how pain-bad 0)
    2 ((How do you rate your pain ?) (pain-description)) (0 :gist)
  ; Can you tell me about your pain?
  1 (0 pain-info 1 about 2 pain 0)
    2 ((Can you tell me about your pain ?) (pain-description)) (0 :gist)
  1 (0 what pain 1 be you 0)
    2 ((Can you tell me about your pain ?) (pain-description)) (0 :gist)
  ; Where does it hurt?
  1 (0 where be 3 pain 0)
    2 ((Where is the pain located ?) (pain-description)) (0 :gist)
  1 (0 what part 3 pain 0)
    2 ((Where is the pain located ?) (pain-description)) (0 :gist)
  ; Does it hurt to [...]
  1 (0 do 2 pain to 0)
    2 (0 breath 0)
      3 ((Does it hurt to breath ?) (pain-description)) (0 :gist)
    2 (0)
      3 ((Does it hurt to do anything ?) (pain-description)) (0 :gist)
))


(READRULES '*pain-reaction*
'(

))