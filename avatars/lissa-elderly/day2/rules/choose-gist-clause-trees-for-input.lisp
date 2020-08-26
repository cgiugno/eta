;; File for setting up the choice tree *gist-clause-trees*,
;; which selects among specific gist-clause extraction trees
;; for processing a user input, based on a feature-augmented 
;; initial gist clause for Lissa's output (usually a question
;; to the user that prompted the input).
;;
;; The following Lissa gist clauses are from "lissa5-schema.lisp",
;; where 'store-output-gist-clauses' is used to store output
;; gist clauses in a hash table indexed by action proposition
;; variables, and the hash table itself is accessed as property
;; 'gist-clauses' of *lissa-schema*:
;;
;;	How long have you been in Rochester
;;	What do you like most about Rochester?
;;	And what do you not like about it?
;;	Is there anything you would change?
;;
;;   While these are hand-supplied, the idea is that ultimately
;;   they would be set up automatically via NLP. For example,
;;   "Did you find it hard?" might readily lead to gist clause
;;   "Did you find your favorite class hard?" if we can successfully
;;   resolve the anaphor. More subtly, a trailing question "What
;;   about you?" often seems interpretable as a kind of analogical
;;   variant of the speaker's preceding assertion, just changing
;;   a nominal (etc.) phrase -- e.g., "I'm a comp sci major"  --> 
;;   "I'm a ... major"; "My favorite movie is Bladerunner"  --> 
;;   "My favorite movie is ...", etc.
;;
;;   This is why we don't simply use a verbatim match to Lissa's
;;   output.

(READRULES '*gist-clause-trees-for-input*
'(
   ; rochester1
   1 (2 how long 6 in Rochester 0)
      2 (*how-long-in-rochester-input*) (0 :subtrees)
   1 (6 you not 2 like 4 Rochester 1) 
      2 (*not-like-about-rochester-input*) (0 :subtrees)
   1 (2 what 2 you like 4 Rochester 0)
      2 (*like-about-rochester-input*) (0 :subtrees)
   1 (2 what 2 you 2 change 4 Rochester 0)
      2 (*changing-rochester-input*) (0 :subtrees)

   ; rochester2
   1 (2 what 3 we 1 do 4 tour of Rochester 0)
      2 (*tour-of-rochester-input* 
         *thematic-tour-of-rochester-input*) (0 :subtrees)
   1 (2 what 2 your favorite restaurant 4 in Rochester 0)
      2 (*favorite-eatery-input*) (0 :subtrees)
   1 (2 what 3 is 2 garbage plate 0)
      2 (*garbage-plate-input*
         *thematic-garbage-plate-input*) (0 :subtrees)
   1 (3 you 5 Dinosaur Barbecue 0)
      2 (*dinosaur-bbq-input*) (0 :subtrees)

   ; pets
   1 (2 do 1 have 1 pet 3)
      2 (*have-a-pet-input*) (0 :subtrees)
   1 (1 Tell me about 2 pet 3 family 2 neighbor 3) 
      2 (*family-neighbor-pet-input*) (0 :subtrees)
   1 (1 how 2 pets help 3)
      2 (*pets-help-owners-input*) (0 :subtrees)
))
