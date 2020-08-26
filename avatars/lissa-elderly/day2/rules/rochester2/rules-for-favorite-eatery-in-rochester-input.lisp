(MAPC 'ATTACHFEAT
'(
   (rochester-restaurant-t Tahou Tahou\'s Tahoe Tahoe\'s plate Barbecue BBQ Luck Rosita\'s town star)
   (restaurant-type restaurant-type-one restaurant-type-two)
   (restaurant-type-one Asian Chinese Mexican Japanese Italian Korean Peruvian Thai Ethiopian Indian Mediterranean Greek Cambodian German Polish Spanish Belgian) 
   (restaurant-type-two burger pizza grill grills) 
   (restaurant-type-three diner cafe)
))
   

(READRULES '*favorite-eatery-input*
'(
   ; Reciprocal questions
   1 (0 wh_ 1 you 0 ?)
      2 (What is your favorite eatery in Rochester ?) (0 :gist)
   1 (0 you heard 0 ?)
      2 (Have you heard of that restaurant ?) (0 :gist)
   1 (0 you been 0 ?)
      2 (Have you been to that restaurant ?) (0 :gist)

   ; Specific answers
   1 (0 rochester-restaurant rochester-restaurant-t 0)
      2 (0 Dinosaur Barbecue 0)
         3 ((My favorite place to eat is Dinosaur Barbecue \.) (Rochester-eateries dinosaur)) (0 :gist)
      2 ((My favorite place to eat is 2 3 \.) (Rochester-eateries)) (0 :gist)
   1 (0 rochester-restaurant 0)
      2 (0 Dinosaur 0)
         3 ((My favorite place to eat is Dinosaur Barbecue \.) (Rochester-eateries dinosaur)) (0 :gist)
      2 ((My favorite place to eat is 2 \.) (Rochester-eateries)) (0 :gist)
   1 (0 restaurant-type 0)
      2 ((My favorite place to eat is an 2 restaurant \.) (Rochester-eateries)) (0 :gist)

   ; Unbidden answers
   ;; 1 (0 dinosaur 0)
   ;;    2 ((I have been to Dinosaur Barbecue \.) (dinosaur)) (0 :gist)
   ;; 1 (0 garbage plate 0)
   ;;    2 ((I have had a garbage plate \.) (garbage-plate)) (0 :gist)
   ;; 1 (0 Nick Tahou\'s 0)
   ;;    2 ((I have had a garbage plate \.) (garbage-plate)) (0 :gist)
   ;; 1 (0 Steve T\'s 0)
   ;;    2 ((I have had a garbage plate \.) (garbage-plate)) (0 :gist)
      
   1 (0) 
      2 ((NIL Gist \: nothing found for where My favorite place to eat is \.) (Rochester-eateries)) (0 :gist)
))


(READRULES '*reaction-to-favorite-eatery-input*
'(
   1 (0 rochester-restaurant 0)
      2 (You have not been there\, but it sounds nice\.) (100 :out)
   1 (0 restaurant-type 0)
      2 (0 restaurant-type-one 0)  ;; international
         3 (That\'s great\. You love 2 food\.) (100 :out)
      2 (0 restaurant-type-two 0)  ;; grill pizza etc
         3 (0 grill 0)
            4 (You are actually a fan of grills\.) (100 :out) 
         3 (You are actually a fan of 2 \.) (100 :out)
      2 (0 restaurant-type-three 0) ;; 
         3 (You like that kind of food too\.) (100 :out)

   1 (0 NIL Gist 0)
      2 (You love to experience new places and new tastes\.) (100 :out)
))
