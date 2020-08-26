; This is a small trial pattern base for reacting to the user's
; answer concerning what s/he likes about Rochester.

; We also provide features, supplementing the generic ones in
; "general-word-data.lisp", relevant to the topic here.

(MAPC 'ATTACHFEAT
'(
  ;; (many five six seven eight nine ten eleven twelve thirteen fourteen
  ;;   fifteen sixteen seventeen eighteen nineteen twenty)
	(several_ two three four)	   
  (month months)
		  
	(few a one two three four five six seven eight nine ten eleven)
  (many twelve thirteen fourteen fifteen sixteen seventeen eighteen nineteen twenty)
  (lots thirty forty fifty sixty seventy eighty ninety hundred)	  	  
))    


(READRULES '*how-long-in-rochester-input*
'(
  ; Reciprocal questions
  1 (0 what about 0 you 0 ?)
	  2 (How long have you been here in Rochester ?) (0 :gist)
  1 (0 how about 0 you 0 ?)
    2 (How long have you been here in Rochester ?) (0 :gist)
	1 (0 how long 2 been 0 in Rochester 0 ?)
    2 (How long have you been here in Rochester ?) (0 :gist)

  ; Specific answers
  1 (0 life 0)
    2 ((I have been in Rochester my whole life \.) (time-in-rochester)) (0 :gist)
  1 (0 few years 0)
    2 (0 lots 1 years 0)
	    3 ((I have been in Rochester for more than 2 years \.) (time-in-rochester)) (0 :gist)
    2 (0 few years 0)
	    3 ((I have been in Rochester for 2 years \.) (time-in-rochester)) (0 :gist)
  1 (0 many years 0)
    2 ((I have been in Rochester for 2 years \.) (time-in-rochester)) (0 :gist)
  1 (0 several_ years 0)
    2 ((I have been in Rochester for 2 years \.) (time-in-rochester)) (0 :gist)
  1 (0 years 0)
    2 ((I have been in Rochester for several years \.) (time-in-rochester)) (0 :gist)
  1 (0 year 0)
    2 ((I have been in Rochester for almost a year \.) (time-in-rochester)) (0 :gist)
  1 (0 month 0)
    2 ((I have been in Rochester for several months \.) (time-in-rochester)) (0 :gist)

  1 (0) 
	  2 ((NIL Gist \: nothing found for how long I have been in Rochester \.) (time-in-rochester)) (0 :gist)
))


(READRULES '*reaction-to-how-long-in-rochester-input*  
'(
  1 (0 whole life 0)
    2 (Wow\, that is longer than you have been anywhere\!) (100 :out)
  1 (0 many years 0)
    2 (Wow\, that is longer than you have been anywhere\!) (100 :out)
  1 (0 lots years 0)
    2 (Wow\, that is longer than you have been anywhere\!) (100 :out)
  1 (0 several years 0)
    2 (So I have been here for a while\.) (100 :out)
  1 (0 few years 0)
    2 (So I have been here for a while\.) (100 :out)
  1 (0 a year 0)
    2 (So I am pretty new here\.) (100 :out)
    
	1 (0 NIL Gist 0)
    2 (So I know something about Rochester\.) (100 :out)
)); end of *reaction-to-how-long-in-rochester-input*

