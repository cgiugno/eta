;; (Do you live by yourself or with others ?)
;;	(live-alone)
;;		from-live-alone-input
;;			(0 I live by myself 0) (0 I live with  0)
;;			gist-question:(3 do you live 2 yourself 0)

(MAPC 'ATTACHFEAT
'(
   (live living)
   (spouse husband wife)
	(mother mom)
	(father dad)
))


(READRULES '*live-alone-input*
'(
   ; Reciprocal questions
   1 (0 what 2 you 0 ?)
      2 (do you live by yourself ?) (0 :gist)
   1 (0 how 2 you 0 ?)
      2 (do you live by yourself ?) (0 :gist)
	1 (0 do you live 0 ?)
      2 (do you live by yourself ?) (0 :gist)

   ; Specific answers
   1 (0 by myself 0) 
      2 ((I live by myself \.)  (live-alone)) (0 :gist) 		
   1 (0 live alone 0) 
      2 ((I live by myself \.)  (live-alone)) (0 :gist) 		
   1 (0 live 2 family 0) 
      2 ((I live with my family \.)  (live-alone)) (0 :gist) 		
   1 (0 live 2 spouse 0) 
      2 ((I live with my spouse \.)  (live-alone)) (0 :gist) 		
   1 (0 spouse 3 live 0) 
      2 ((I live with my spouse \.)  (live-alone)) (0 :gist) 		
   1 (0 live 2 partner 0) 
      2 ((I live with my partner \.)  (live-alone)) (0 :gist) 		
   1 (0 live 2 child 0) 
      2 ((I live with my children \.)  (live-alone)) (0 :gist) 		
   1 (0 children 3 live 0) 
      2 ((I live with my children \.)  (live-alone)) (0 :gist) 		
   1 (0 live 2 mother 0) 
      2 ((I live with my mother \.)  (live-alone)) (0 :gist) 		
   1 (0 mother 3 live 0) 
      2 ((I live with my mother \.)  (live-alone)) (0 :gist) 		
   1 (0 live 2 friend 0) 
      2 ((I live with my friend \.)  (live-alone)) (0 :gist) 		
   1 (0 friend 3 live 0) 
      2 ((I live with my mother \.)  (live-alone)) (0 :gist) 		
   1 (0 live 2 bg-friend 0) 
      2 ((I live with my 4 \.)  (live-alone)) (0 :gist) 		
   1 (0 bg-friend 3 live 0) 
      2 ((I live with my 4 \.)  (live-alone)) (0 :gist) 		

   1 (0)
      2 ((NIL Gist \: nothing found for who I live with \.) (live-alone)) (0 :gist)
))


(READRULES '*reaction-to-live-alone-input*
'(
   1 (0 myself 0)	
      2 (Lots of people live alone\. There are good things about having my own space\.) (100 :out)
   1 (0 spouse 0)
      2 (It\'s good I live together and can look out for each other\.) (100 :out)
   1 (0 partner 0)
      2 (It\'s good I live together and can look out for each other\.) (100 :out)
   1 (0 family 0)
      2 (It\'s good to live with my family\. They must be nice\.) (100 :out)
   1 (0 child 0)
      2 (It\'s good to live with my children\. They must be nice\.) (100 :out)
   1 (0 friend 0)
      2 (It is nice to live with a good friend \.) (100 :out)
   1 (0 bg-friend 0)
      2 (Oh\, that\'s cool \.) (100 :out)
   1 (0 mother 0)
      2 (That\'s nice\. You have never lived with my parents since you got married\. Sometimes you miss them a lot\.) (100 :out)

   1 (0 NIL Gist 0)
      2 (You find that some people prefer to live alone\, and some with others\. It really just depends on preference\.) (100 :out)
))
