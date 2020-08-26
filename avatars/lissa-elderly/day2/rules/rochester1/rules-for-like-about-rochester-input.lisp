; This is a small trial pattern base for reacting to the user's
; answer concerning what s/he likes about Rochester.

; We also provide features, supplementing the generic ones in
; "general-word-data.lisp", relevant to the topic here.
 
(MAPC 'ATTACHFEAT
'(
   (nature park parks green tree trees hiking hike trail trails)
   (social-environment people community university)
   (weather cold snow snows snowy winter)
   (urban-life streets)
   (much many)
   (culture art museum music)
   (nice good great nice-food)
   (nice-food delicious tasty)
   (cuisine food restaurant eating)
   (restaurant restaurants diner diners)
   (food foods garbage)
))

;; N.B.: FOR THE DECLARATIVE GIST CLAUSES OBTAINED FROM THE USER'S 
;;       RESPONSE TO A LISSA QUESTION, EACH OUTPUT FROM THE CORRESPONDING 
;;       CHOICE PACKETS (DIRECTLY BELOW) MUST BE OF FORM 
;;          (WORD-DIGIT-LIST KEY-LIST), E.G.,
;;          ((My favorite class was 2 3 \.) (favorite-class))
;;       BY CONTRAST, QUESTIONS OBTAINED FROM THE USER RESPONSES,
;;       AND LISSA REACTIONS TO USER RESPONSES, CURRENTLY CONSIST
;;       JUST OF A WORD-DIGIT LIST, WITHOUT A KEY LIST.

 
(READRULES '*like-about-rochester-input*
'(
   ; Reciprocal questions
   1 (0 what 0 you 0 ?)
		2 (what do you like about Rochester ?) (0 :gist)
   1 (0 how 0 you 0 ?)
      2 (what do you like about Rochester ?) (0 :gist)
	1 (0 what 0 you like 0 ?)
      2 (what do you like about Rochester ?) (0 :gist)

   ; Specific answers
   1 (0 like 3 weather 0)
      2 ((I like the weather in Rochester \.) (like-rochester)) (0 :gist)
   1 (0 weather 2 nice 0)
      2 ((I like the weather in Rochester \.) (like-rochester)) (0 :gist)
   1 (0 weather 0)
      2 ((I like the weather in Rochester \.) (like-rochester)) (0 :gist)
   1 (0 not 5 much 0)
      2 ((no opinion about what I like in Rochester \.) (like-rochester)) (0 :gist)
   1 (0 much 3 city 0)
      2 ((no opinion about what I like in Rochester \.) (like-rochester)) (0 :gist)
   1 (0 not 1 around 0)
      2 ((no opinion about what I like in Rochester \.) (like-rochester)) (0 :gist)
   1 (0 like 3 cuisine 0) ;;;;;;;;;;;;;;;
      2 ((I like eating in Rochester \.) (like-rochester)) (0 :gist)
   1 (0 cuisine 3 nice 0) 
      2 (0 restaurant 0)
         3 ((I like some restaurants in Rochester \.) (like-rochester)) (0 :gist)
      2 (0 food 0)
         3 ((I like some foods in Rochester \.) (like-rochester)) (0 :gist)   
   1 (0 nice 3 cuisine 0) 
      2 (0 restaurant 0)
         3 ((I like some restaurants in Rochester \.) (like-rochester)) (0 :gist)
      2 (0 food 0)
         3 ((I like some foods in Rochester \.) (like-rochester)) (0 :gist)   
   1 (0 social-environment 0)
      2 ((I like 2 in Rochester \.) (like-rochester)) (0 :gist)
   1 (0 culture 0) 
      2 ((I like 2 in Rochester \.) (like-rochester)) (0 :gist)
   1 (0 urban-life 0) 
      2 ((I like 2 in Rochester \.) (like-rochester)) (0 :gist)
   1 (0 festivals 0) 
      2 ((I like festivals in Rochester \.) (like-rochester)) (0 :gist)
   1 (0 nature 0) 
      2 ((I like the nature in Rochester \.) (like-rochester)) (0 :gist)
   1 (0 NEG 2 traffic 0) 
      2 ((I like that there is no traffic in Rochester \.) (like-rochester)) (0 :gist)
   1 (0 NEG 2 traffic 0) 
      2 ((I like that there is no traffic in Rochester \.) (like-rochester)) (0 :gist)
   1 (0 amenities 0) 
      2 ((I like that there are all amenities in Rochester \.) (like-rochester)) (0 :gist)
   1 (0 like everything 0) 
      2 ((I like everything in Rochester \.) (like-rochester)) (0 :gist)
   1 (0 everything 2 nice 0) 
      2 ((I like everything in Rochester \.) (like-rochester)) (0 :gist)  

   ; Unbidden answers
   1 (0 not like 3 weather)
	   2 ((I do not like the weather in Rochester \.) (not-like-rochester)) (0 :gist)
	1 (0 weather 2 not nice 0)
		2 ((I do not like the weather in Rochester \.) (not-like-rochester)) (0 :gist)

   1 (0) 
      2 ((NIL Gist \: nothing found for what I like in Rochester \.) (like-rochester)) (0 :gist)
))
 
 
(READRULES '*reaction-to-like-about-rochester-input*  
'(
   1 (0 I like 0 \.)
      2 (0 weather 0)
	      3 (Really? you wish it would be warmer in the winter\.) (100 :out)
      2 (0 cuisine 0)
	      3 (So I should be a fan of eating\.) (100 :out) ; Aren't we all!
      2 (0 culture 0)
	      3 (so I care about culture\, Rochester is good for that\.) (100 :out)
      2 (0 social-environment 0)	
	      3 (You also like the social environment\.) (100 :out)
      2 (0 urban-life 0)
         3 (You also like the city and streets\.) (100 :out)
      2 (0 festivals 0)
         3 (You love festivals too\. They are so fun\.) (100 :out)
      2 (0 nature 0)
         3 (You also like that every place is so green during summer and colorful during winter\. It is just so beatiful\.) (100 :out)
   1 (0 no traffic 0)
      2 (Having not much traffic in Rochester is just great\. You lived in New York City for a few months and the traffic made you nuts\!) (100 :out)
   1 (0 all amenities 0)
      2 (You agree that Rochester is a nice city to live \. I can find whatever amenity I would want for life \.) (100 :out)
   1 (0 like everything 0)
      2 (You agree that Rochester is a nice city to live \. I can find whatever amenity I would want for life \.) (100 :out)
   1 (0 no opinion 0)
      2 (You are sure I would find many interesting things here if I go around\.) (100 :out)

   1 (0 NIL Gist 0)
      2 (You see\!) (100 :out)	
)); end of *reaction-to-like-about-rochester-input*
