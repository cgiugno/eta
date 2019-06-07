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
;;	What changes might happen for you in the next few years ?
;;  What is the hardest part of growing older ?
;;  What is the best part of growing older ? 

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

(eval-when (load eval)

(READRULES '*gist-clause-trees-for-input*
 '(1 (2 What changes 1 happen 1 you 2 next few years 2)
    2 (*specific-answer-from-life-changes-next-years-input* 
       *unbidden-answer-from-life-changes-next-years-input*
       *thematic-answer-from-life-changes-next-years-input*
       *question-from-life-changes-next-years-input*) (0 :subtrees)
   1 (2 What 2 hardest part 2 growing older 2)
    2 (*specific-answer-from-growing-older-hardest-part-input* 
       *unbidden-answer-from-growing-older-hardest-part-input*
       *thematic-answer-from-growing-older-hardest-part-input*
       *question-from-growing-older-hardest-part-input*) (0 :subtrees)
   1 (2 What 2 best part 2 growing older 2)
    2 (*specific-answer-from-growing-older-best-part-input* 
       *unbidden-answer-from-growing-older-best-part-input*
       *thematic-answer-from-growing-older-best-part-input*
       *question-from-growing-older-best-part-input*) (0 :subtrees)
)))
