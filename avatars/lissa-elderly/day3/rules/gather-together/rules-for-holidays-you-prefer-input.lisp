;; (Are there other holidays you prefer ?)
;;	(holidays-you-prefer)
;;		from-holidays-you-prefer-input
;;			(0 The holiday I prefer is 0)
;;			gist-question:(3 are there 2 holidays you prefer 0)

(MAPC 'ATTACHFEAT
'(
   (american-holidays Christmas Halloween Thanksgiving Easter)
   (fourth 4th)
))


(READRULES '*holidays-you-prefer-input*
'(
   ; Reciprocal question
   1 (0 what 2 you 0 ?)
      2 (are there other holidays you prefer ?) (0 :gist)
   1 (0 how 2 you 0 ?)
      2 (are there other holidays you prefer ?) (0 :gist)
   1 (0 wh_ 2 holidays 2 do you 0 ?)
      2 (are there other holidays you prefer ?) (0 :gist)
   1 (0 holidays you 0 ?)
      2 (are there other holidays you prefer ?) (0 :gist)

   ; Specific answer
   1 (NEG 0) 
      2 ((I so not know what the holiday I prefer is \.)  (holidays-you-prefer)) (0 :gist) 		
   1 (0 american-holidays 0) 
      2 ((The holiday I prefer is 2 \.)  (holidays-you-prefer)) (0 :gist) 		
   1 (0 Independence day 0) 
      2 ((The holiday I prefer is 2 3 \.)  (holidays-you-prefer)) (0 :gist) 		
   1 (0 New year\'s 0) 
      2 ((The holiday I prefer is New year\'s \.)  (holidays-you-prefer)) (0 :gist) 		
   1 (0 fourth 1 July 0) 
      2 ((The holiday I prefer is fourth of July \.)  (holidays-you-prefer)) (0 :gist) 		
   1 (0 mother\'s day 0) 
      2 ((The holiday I prefer is mother\'s day \.)  (holidays-you-prefer)) (0 :gist) 		
   1 (0 father\'s day 0) 
      2 ((The holiday I prefer is father\'s day \.)  (holidays-you-prefer)) (0 :gist) 		
   1 (0 valentine\'s day 0) 
      2 ((The holiday I prefer is valentine \.)  (holidays-you-prefer)) (0 :gist) 
   1 (0 valentine 0) 
      2 ((The holiday I prefer is valentine \.)  (holidays-you-prefer)) (0 :gist) 	

   1 (0)
      2 ((NIL Gist \: nothing found for what the holiday I prefer is \.) (holidays-you-prefer)) (0 :gist)
))


(READRULES '*reaction-to-holidays-you-prefer-input*
'(
   1 (0 american-holidays 0)
      2 (0 Christmas 0)
         3 (You love Christmas\, seeing all the lights makes you happy\.) (100 :out)
      2 (0 Halloween 0)
         3 (Halloween is so much fun\! All dressing up and stuff always make a great night\.) (100 :out)
      2 (0 Thanksgiving 0)
         3 (Thanksgiving is a great holiday in your opinion\, since you get to see all your family \.) (100 :out)
      2 (0 Easter 0)
         3 (Easter is a fun holiday\, you like watching people do egg scavanger hunts\.) (100 :out)
   1 (0 independence 0)
      2 (You always enjoy the Fourth of July fireworks\.) (100 :out)
   1 (0 new year\'s 0)
      2 (You like celebrating a new year\, a fresh start\.) (100 :out)
   1 (0 fourth 0)
      2 (You always enjoy the Fourth of July fireworks\.) (100 :out)
   1 (0 mother\'s 0)
      2 (It\'s nice to have a day to think of the parents\.) (100 :out)
   1 (0 father\'s 0)
      2 (It\'s nice to have a day to think of the parents\.) (100 :out)
   1 (0 valentine 0)
      2 (well\, Valentine\'s day is too romantic for you \, it is more for young people you think \.) (100 :out)
   1 (0 I do not know 0)
      2 (Oh\, okay\. You personally love Christmas\, seeing all the lights makes you happy\.) (100 :out)
      
   1 (0 NIL Gist 0)
      2 (You personally love Christmas\, seeing all the lights makes you happy\.) (100 :out)
))
