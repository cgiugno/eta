
 
(MAPC 'ATTACHFEAT
'(
  (gp-ingredients sauce macaroni potato cheese mustard ketchup burger hamburger fries)
))

; Since the question is open-ended, we're not really looking for specific answer.
; Gist clauses will be derived from the thematic answer rules.

(READRULES '*garbage-plate-input*
'(
  ; Reciprocal questions
  1 (0 have you 2 tried it 0 ?)
	  2 ((Have you tried garbage plate yet ?) (garbage-plate)) (0 :gist)
  1 (0 have you 2 tried 2 garbage plate 0 ?)
	  2 ((Have you tried garbage plate yet ?) (garbage-plate)) (0 :gist)

  ; Specific answers
  1 (0 NEG idea 0)
    2 ((I do not know about garbage plate \.) (garbage-plate)) (0 :gist)
  1 (0 have NEG tried 0)
    2 ((I have not tried garbage plate \.) (garbage-plate)) (0 :gist)
  1 (0 have never tried 0)
    2 ((I have not tried garbage plate \.) (garbage-plate)) (0 :gist)
  1 (0 have 3 NEG 3 had 0)
    2 ((I have not tried garbage plate \.) (garbage-plate)) (0 :gist)
  1 (0 have 3 never 3 had 0)
    2 ((I have not tried garbage plate \.) (garbage-plate)) (0 :gist)
  1 (0 NEG tested 0)
    2 ((I have not tried garbage plate \.) (garbage-plate)) (0 :gist)
  1 (0 have never tested 0)
    2 ((I have not tried garbage plate \.) (garbage-plate)) (0 :gist)
  1 (0 have 3 NEG 3 been 0)
    2 ((I have not tried garbage plate \.) (garbage-plate)) (0 :gist)
  1 (0 gp-ingredients 0)
    2 ((I told you garbage plate ingredients \.) (garbage-plate)) (0 :gist)
  1 (0 NEG know 0)
    2 ((I do not know about garbage plate \.) (garbage-plate)) (0 :gist)
  1 (0 NEG like 0)
    2 ((I do not like garbage plate \.) (garbage-plate)) (0 :gist)	

  1 (0) 
    2 ((NIL Gist \: nothing found for what garbage plate is \.) (garbage-plate)) (0 :gist)
))
		

(READRULES '*thematic-garbage-plate-input*
'(
  1 (0 BADPRED 0)
    2 ((I do not like garbage plate \.) (garbage-plate)) (0 :gist)
  1 (0 NEG GOODPRED 0)
    2 ((I do not like garbage plate \.) (garbage-plate)) (0 :gist)
  1 (0 NEG 1 GOODPRED 0)
    2 ((I do not like garbage plate \.) (garbage-plate)) (0 :gist)
  1 (0 GOODPRED 0)
    2 ((I like garbage plate \.) (garbage-plate)) (0 :gist)
))


(READRULES '*reaction-to-garbage-plate-input*
'(
  1 (0 I do not know 0)
    2 (Well\, I should try it at least once\. You will definitely check it out some time\. ) (100 :out)
  1 (0 I have not tried 0)
    2 (Well\, I should try it at least once\. You will definitely check it out some time\. ) (100 :out)
  1 (0 I told you 0 ingredients 0)
    2 (Wow\, that sounds like a funny idea\. You will definitely try it some time\.) (100 :out)
  1 (0 NEG like 0)
    2 (You do not know if you like it but you are actually curious to try it\.) (100 :out)
    
  1 (0 NIL Gist 0) 
    2 (You love try new stuff\. You should give it a try\.) (100 :out)
)); end of *reaction-to-garbage-plate-input*
