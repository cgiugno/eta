(eval-when (load eval)
  (MAPC 'ATTACHFEAT
  '((cleaning-verb cleaning-verb-pres cleaning-verb-prog)
    (cleaning-verb-pres dust sweep vacuum wipe wash clean mop)
    (cleaning-verb-prog dusting sweeping vacuuming wiping washing cleaning mopping)
    (cleaning-noun dishes furniture bedding car cars floors window windows pet carpet bathrooms refrigerator blind blinds 
      curtain carpet garage oven pillow pillows quilt clothes sheets towels towel floor)
    (arrange arranging)
    (pet pets cat cats dog dogs)
    (Feeding feed pets)
    (Doing laundry laundries)
    (Preparing prepare meals cooking)
    (Watering water plants plant)
    (Mowing mow the lawn)
    (weeding weed garden)
    (Bathing bath pets)
    (Prune pruning trees shrubs)
    (like enjoy)
    (household chore households chores)
    (dislike hate)
    (like-verbs like love prefer)
  ))
   
;; 	Most of us like some chores more than others. What’s the chore you enjoy the most?
;;	householdChore-enjoy
;;	(0 household chore 4 I enjoy 0) (0 household chore 4 I do not enjoy 0)
;;	(What is the household chore you enjoy the most ?)
;;	(3 what household chore 2 enjoy 3)
	

(READRULES '*specific-answer-from-householdChore-enjoy-input*
  '( 1 (1 NEG 1 like 2 household 0) ;; e.g. I don't really like doing any household
        2 ((Every household chore I do not enjoy \.)  (householdChore-enjoy)) (0 :gist) ;; new gist clause format needed
    ;==================================================================================
     1 (0 dislike 2 cleaning-verb cleaning-noun 0) 
        2 ((4 5 is the household chore that I do not enjoy at all \.)  (householdChore-enjoy)) (0 :gist)
     1 (0 dislike 2 feeding 1 pets 0) 
        2 ((Feeding 6 is the household chore that I do not enjoy at all \.)  (householdChore-enjoy)) (0 :gist)
     1 (0 dislike 2 doing laundry 0) 
        2 ((4 5 is the household chore that I do not enjoy at all \.)  (householdChore-enjoy)) (0 :gist)
     1 (0 dislike 2 preparing meals 0) 
        2 ((4 5 is the household chore that I do not enjoy at all \.)  (householdChore-enjoy)) (0 :gist)
     1 (0 dislike 2 watering plants 0) 
        2 ((4 5 is the household chore that I do not enjoy at all \.)  (householdChore-enjoy)) (0 :gist)
     1 (0 dislike 2 weeding garden 0) 
        2 ((4 5 is the household chore that I do not enjoy at all \.)  (householdChore-enjoy)) (0 :gist)
     1 (0 dislike 2 bathing 1 pets 0) 
        2 ((4 6 is the household chore that I do not enjoy at all \.)  (householdChore-enjoy)) (0 :gist)
     1 (0 dislike 2 pruning trees 0) 
        2 ((4 5 is the household chore that I do not enjoy at all \.)  (householdChore-enjoy)) (0 :gist)
     1 (0 dislike 2 cooking 0) 
        2 ((4 is the household chore that I do not enjoy at all \.)  (householdChore-enjoy)) (0 :gist)
     1 (0 dislike 2 arranging 0) 
        2 ((4 is the household chore that I do not enjoy at all \.)  (householdChore-enjoy)) (0 :gist)
     1 (0 dislike 2 laundry 0) 
        2 ((Doing 4 is the household chore that I do not enjoy at all \.)  (householdChore-enjoy)) (0 :gist)
     1 (0 dislike 2 cleaning-verb 0) 
        2 ((4 is the household chore that I do not enjoy at all \.)  (householdChore-enjoy)) (0 :gist)
    ;==================================================================================
     1 (0 NEG like 4 cleaning-verb cleaning-noun 0) 
        2 ((5 6 is the household chore that I do not enjoy at all \.)  (householdChore-enjoy)) (0 :gist)
     1 (0 NEG like 4 feeding 1 pets 0) 
        2 ((Feeding 7 is the household chore that I do not enjoy at all \.)  (householdChore-enjoy)) (0 :gist)
     1 (0 NEG like 4 doing laundry 0) 
        2 ((5 6 is the household chore that I do not enjoy at all \.)  (householdChore-enjoy)) (0 :gist)
     1 (0 NEG like 4 preparing meals 0) 
        2 ((5 6 is the household chore that I do not enjoy at all \.)  (householdChore-enjoy)) (0 :gist)
     1 (0 NEG like 4 watering plants 0) 
        2 ((5 6 is the household chore that I do not enjoy at all \.)  (householdChore-enjoy)) (0 :gist)
     1 (0 NEG like 4 weeding garden 0) 
        2 ((5 6 is the household chore that I do not enjoy at all \.)  (householdChore-enjoy)) (0 :gist)
     1 (0 NEG like 4 bathing 1 pets 0) 
        2 ((5 7 is the household chore that I do not enjoy at all \.)  (householdChore-enjoy)) (0 :gist)
     1 (0 NEG like 4 pruning trees 0) 
        2 ((5 6 is the household chore that I do not enjoy at all \.)  (householdChore-enjoy)) (0 :gist)
     1 (0 NEG like 4 cooking 0) 
        2 ((5 is the household chore that I do not enjoy at all \.)  (householdChore-enjoy)) (0 :gist)
     1 (0 NEG like 4 arranging 0) 
        2 ((5 is the household chore that I do not enjoy at all \.)  (householdChore-enjoy)) (0 :gist)
     1 (0 NEG like 4 laundry 0) 
        2 ((Doing 5 is the household chore that I do not enjoy at all \.)  (householdChore-enjoy)) (0 :gist)
     1 (0 NEG like 4 cleaning-verb 0) 
        2 ((5 is the household chore that I do not enjoy at all \.)  (householdChore-enjoy)) (0 :gist)
    ;==================================================================================
     1 (0 like-verbs 3 cleaning-verb cleaning-noun 0) 
        2 ((4 5 is the household chore that I enjoy the most \.)  (householdChore-enjoy)) (0 :gist)
     1 (0 like-verbs 3 feeding 1 pets 0) 
        2 ((Feeding 6 is the household chore that I enjoy the most \.)  (householdChore-enjoy)) (0 :gist)
     1 (0 like-verbs 3 doing laundry 0) 
        2 ((4 5 is the household chore that I enjoy the most \.)  (householdChore-enjoy)) (0 :gist)
     1 (0 like-verbs 3 preparing meals 0) 
        2 ((4 5 is the household chore that I enjoy the most \.)  (householdChore-enjoy)) (0 :gist)
     1 (0 like-verbs 3 watering plants 0) 
        2 ((4 5 is the household chore that I enjoy the most \.)  (householdChore-enjoy)) (0 :gist)
     1 (0 like-verbs 3 weeding garden 0) 
        2 ((4 5 is the household chore that I enjoy the most \.)  (householdChore-enjoy)) (0 :gist)
     1 (0 like-verbs 3 bathing 1 pets 0) 
        2 ((4 6 is the household chore that I enjoy the most \.)  (householdChore-enjoy)) (0 :gist)
     1 (0 like-verbs 3 pruning trees 0) 
        2 ((4 5 is the household chore that I enjoy the most \.)  (householdChore-enjoy)) (0 :gist)
     1 (0 like-verbs 3 cooking 0) 
        2 ((4 is the household chore that I enjoy the most \.)  (householdChore-enjoy)) (0 :gist)
     1 (0 like-verbs 3 arranging 0) 
        2 ((4 is the household chore that I enjoy the most \.)  (householdChore-enjoy)) (0 :gist)
     1 (0 like-verbs 3 laundry 0) 
        2 ((Doing 4 is the household chore that I enjoy the most \.)  (householdChore-enjoy)) (0 :gist)
     1 (0 like-verbs 3 cleaning-verb 0) 
        2 ((4 is the household chore that I enjoy the most \.)  (householdChore-enjoy)) (0 :gist)
     1 (0)
       2 ((NIL Gist \: nothing found for the household chore I enjoy \.) (householdChore-enjoy)) (0 :gist)
))
       
       
 (READRULES '*thematic-answer-from-householdChore-enjoy-input*
  '(

  ))

 (READRULES '*unbidden-answer-from-householdChore-enjoy-input*
  '( 
  ))
		
 (READRULES '*question-from-householdChore-enjoy-input*
  '(  1 (0 what 4 you 0)
        2 (What is the household chore you enjoy the most ?) (0 :gist)
      1 (0 how 2 you 2 chores 0)
        2 (How can you do chores ?) (0 :gist)
      1 (0 wh_ 4 chore 1 enjoy 0)
        2 (What is the household chore you enjoy the most ?) (0 :gist)
  ))

(READRULES '*reaction-to-householdChore-enjoy-input*
  '( 1 (0 NEG enjoy 0) 
        2 (You are not a fan of laundries\, but to you it is an important chore to do\.) (100 :out)
     1 (0 cleaning-verb 1 cleaning-noun 0) ;; e.g. wash the floor
        2 (0 cleaning-verb-pres 1 cleaning-noun 0)
          3 (0 wipe 1 cleaning-noun 0)
            4 (Wiping 4 sounds kind of boring to you too but it is important to keep your house tidy\.) (100 :out)
          3 (0 dust 1 cleaning-noun 0)
            4 (Dusting 4 sounds kind of boring to you too but it is important to keep your house tidy\.) (100 :out)
          3 (0 dust 1 cleaning-noun 0)
            4 (Dusting 4 sounds kind of boring to you too but it is important to keep your house tidy\.) (100 :out)
          3 (0 sweep 1 cleaning-noun 0)
            4 (Sweeping 4 sounds kind of boring to you too but it is important to keep your house tidy\.) (100 :out)
          3 (0 vacuum 1 cleaning-noun 0)
            4 (Vacuuming 4 sounds kind of boring to you too but it is important to keep your house tidy\.) (100 :out)
          3 (0 wash 1 cleaning-noun 0)
            4 (Washing 4 sounds kind of boring to you too but it is important to keep your house tidy\.) (100 :out)
          3 (0 clean 1 cleaning-noun 0)
            4 (Cleaning 4 sounds kind of boring to you too but it is important to keep your house tidy\.) (100 :out)
          3 (0 mop 1 cleaning-noun 0)
            4 (Mopping 4 sounds kind of boring to you too but it is important to keep your house tidy\.) (100 :out)
        2 (0 cleaning-verb-prog 1 cleaning-noun 0)
          3 (2 sometimes sounds boring to you too but it is important to keep your house tidy\.) (100 :out)
     1 (0 feeding pets 0) 
        2 (Feeding pets is heart-warming sometimes\. You used to feed homeless cats living around your house\.) (100 :out)
     1 (0 doing laundry 0) 
        2 (You are not a fan of laundries\, but it is an important chore to do\.) (100 :out)
     1 (0 preparing meals 0) 
        2 (You enjoy preparing meals for your family\.) (100 :out)
     1 (0 watering plants 0) 
        2 (You like to chat with your friends or family members when watering plants\.) (100 :out)
     1 (0 weeding garden 0) 
        2 (Sounds nice that I like weeding garden\.) (100 :out)
     1 (0 bathing pets 0) 
        2 (Bathing pets is funny sometimes\. Fluffy animals look completely different right after washing them\.) (100 :out)
     1 (0 pruning trees 0) 
        2 (Pruning trees and enjoy the sunshine can be a good relaxing exercise\.) (100 :out)
     1 (0 cooking 0) 
        2 (You enjoy preparing meals for your family\.) (100 :out)
     1 (0 arranging 0) 
        2 (Arranging things properly can strangely bring you satisfaction\.) (100 :out)
     1 (0 laundry 0) 
        2 (You are not a fan of laundry\, but it is an important chore to do\.) (100 :out)
     1 (0 cleaning-verb 0) 
        2 (0 cleaning-verb-pres 0)
          3 (0 wipe 0)
            4 (Wiping is important to keep the house tidy\.) (100 :out)
          3 (0 dust 0)
            4 (Dusting is important to keep the house tidy\.) (100 :out)
          3 (0 dust 0)
            4 (Dusting is important to keep the house tidy\.) (100 :out)
          3 (0 sweep 0)
            4 (Sweeping is important to keep the house tidy\.) (100 :out)
          3 (0 vacuum 0)
            4 (Vacuuming is important to keep the house tidy\.) (100 :out)
          3 (0 wash 0)
            4 (Washing is important to keep the house tidy\.) (100 :out)
          3 (0 clean 0)
            4 (Cleaning is important to keep the house tidy\.) (100 :out)
          3 (0 mop 0)
            4 (Mopping is important to keep the house tidy\.) (100 :out)
       2 (0 cleaning-verb-prog 0)
         3 (2 is important to keep the house tidy\. ) (100 :out)
     1 (0 NIL Gist 0) 
       2 (You are not a big fan of chores\, but it is a very important thing to do\.) (100 :out)
  ))
); end of eval-when
