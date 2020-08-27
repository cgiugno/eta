;; (where are you from ?)
;;	(hometown)
;;		from-hometown-input
;;			(0 I am from 0)
;;			gist-question:(3 where are you from 0)

(MAPC 'ATTACHFEAT
'(
     (Cities Northern-Cities North-Western-Cities Southern-Cities South-Western-Cities Eastern-Cities South-to-Rochester Central-Cities)
	(Northern-Cities Chicago Indianapolis Columbus Detroit Cleveland)
	(North-Western-Cities Seattle Portland)
	(South-Western-Cities Phoenix Albuquerque)
	(Southern-Cities Houston Dallas Austin Miami Antonio Orleans)
	(South-to-Rochester Atlanta Charlotte)
	(Western-Cities LA Francisco)
	(Eastern-Cities Boston NYC Philadelphia Washington Baltimore Manhathan Brooklyn)
     (Central-Cities Memphis Denver)
	(states Southern-States Eastern-States Western-States Central-States Northern-States other-states)
     (Southern-States Florida Texas Arizona Georgia)
     (Eastern-States Pennsylvania)
	(Western-States California Oregon Washington)
	(Central-States Virginia Carolina Maryland Kansas)
	(Northern-States Ohio Michigan Vermont Massachusetts Minnesota Illinois) 
	(other-states Alabama Alaska Arkansas Colorado Conneticut Delaware Hawaii Idaho Indiana Iowa Kentacky Louisiana Maine Mississippi Missouri Montana Nebraska Nevada Oklahoma Tennessee Utah Wisconsin Wyoming)
	(rochester-towns Fairport Irondequoit Brighton Chili Gates Henrietta Perinton Penfield Pittsford Webster Geneva Greece)
	(states-two-words-1 New South North Rhode West)
	(states-two-words-2 York Jersey Hampshire Mexico Carolina Dakota Island Virginia)
))


(READRULES '*hometown-input*
'(
     ; Reciprocal questions
     1 (0 what 2 you 0 ?)
          2 (where are you from ?) (0 :gist)
     1 (0 how 2 you 0 ?)
          2 (where are you from ?) (0 :gist)
     1 (0 where 2 you from 0 ?)
          2 (where are you from ?) (0 :gist)
     1 (0 where 2 you grow 0 ?)
          2 (where are you from ?) (0 :gist)	

     ; Specific answer   
     1 (0 New York City 0) 
          2 ((I am from 2 3 4 \.)  (hometown)) (0 :gist)
     1 (0 Los Angeles 0) 
          2 ((I am from 2 3 \.)  (hometown)) (0 :gist) 
     1 (0 San Antonio 0) 
          2 ((I am from 2 3 \.)  (hometown)) (0 :gist) 
     1 (0 San Jose 0) 
          2 ((I am from 2 3 \.)  (hometown)) (0 :gist) 
     1 (0 San Diego 0) 
          2 ((I am from 2 3 \.)  (hometown)) (0 :gist) 
     1 (0 I am 2 from rochester 0) 
          2 ((I am from rochester \.)  (hometown)) (0 :gist) 
     1 (0 I am 2 rochesterian 0) 
          2 ((I am from rochester \.)  (hometown)) (0 :gist) 
     1 (0 I grew up in rochester 0) 
          2 ((I am from rochester \.)  (hometown)) (0 :gist) 
     1 (0 local 0)
          2 ((I am from rochester \.)  (hometown)) (0 :gist)
     1 (rochester 0) 
          2 ((I am from rochester \.)  (hometown)) (0 :gist) 
     
     1 (0 states-two-words-1 states-two-words-2 0)
          2 ((I am from 2 3 \.) (hometown)) (0 :gist)
     1 (0 rochester-towns 0) 
          2 ((I am from 2 \.)  (hometown)) (0 :gist) 
     1 (0 Cities 0) 
          2 ((I am from 2 \.)  (hometown)) (0 :gist) 
     1 (0 States 0) 
          2 ((I am from 2 \.)  (hometown)) (0 :gist) 	

     ; Unbidden answer
     1 (0 I am 2 from rochester 0) 
          2 ((I ended up in Rochester since it is my hometown \.)  (endup-in-rochester)) (0 :gist) 
     1 (0 I am 2 rochesterian 0) 
          2 ((I ended up in Rochester since it is my hometown \.)  (endup-in-rochester)) (0 :gist) 
     1 (0 I grew up in rochester 0) 
          2 ((I ended up in Rochester since it is my hometown \.)  (endup-in-rochester)) (0 :gist) 
     1 (0 local 0)
          2 ((I ended up in Rochester since it is my hometown \.)  (endup-in-rochester)) (0 :gist)
     1 (0 rochester-towns 0)        
          2 ((I ended up in Rochester since it is my hometown  \.)  (endup-in-rochester)) (0 :gist)

     1 (0)
          2 ((NIL Gist \: nothing found for where I am from \.) (hometown)) (0 :gist)
))


(READRULES '*reaction-to-hometown-input*
'(
     1 (0 states 0)
          2 (Oh\, You have been to 2 \. It is a nice state\.) (100 :out)
     1 (0 cities 0)
          2 (2 sounds like a fun place to grow up\.) (100 :out)
     1 (0 New York City 0) 
          2 (2 3 4 sounds like a fun place to grow up\.) (100 :out)
     1 (0 Los Angeles 0) 
          2 (2 3 sounds like a fun place to grow up\.)  (100 :out)
     1 (0 San Antonio 0) 
          2 (2 3 sounds like a fun place to grow up\.) (100 :out)
     1 (0 San Jose 0) 
          2 (2 3 sounds like a fun place to grow up\.) (100 :out)
     1 (0 San Diego 0) 
          2 (2 3 sounds like a fun place to grow up\.) (100 :out)
     1 (0 states-two-words-1 states-two-words-2 0)
          2 (0 New York 0)
               3 (Oh\, New York state is your favorite\. A lot of snow in winter and then so green during summer \.) (100 :out)
          2 (Oh\, you have been to 2 3 \. It is a nice state \.) (100 :out)
     1 (0 from rochester 0) 
          2 (It is nice to meet a Rochester native\.) (100 :out)
     1 (0 rochester-towns 0) 
          2 (2 sounds like a fun place to grow up\. A lot of snow in winter and then so green during summer \.) (100 :out)
          
     1 (0 NIL Gist 0)
          2 (It sounds like a nice place to grow up\.) (100 :out)
))
