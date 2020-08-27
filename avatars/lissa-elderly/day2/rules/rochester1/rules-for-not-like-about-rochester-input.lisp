; This is a small trial pattern base for reacting to the user's
; answer concerning what s/he doesn't like about Rochester.

; We also provide features, supplementing the generic ones in
; "general-word-data.lisp", relevant to the topic here.


(MAPC 'ATTACHFEAT
'(
  (old rundown)
  (safety crime dangerous safe neighborhood neighborhoods ward safer)
  (anything nothing)
	(transportation bus buses)
	(weather cold snow snows snowy winter warmer)
))
       
 
(READRULES '*not-like-about-rochester-input*
'(
  ; Reciprocal questions
  1 (0 what 0 you 0 ?)
		2 (what do you not like about Rochester ?) (0 :gist)
  1 (0 how 0 you 0 ?)
    2 (what do you not like about Rochester ?) (0 :gist)
	1 (0 what 2 you not like 0 ?)
    2 (what do you not like about Rochester ?) (0 :gist)

  ; Specific answers
  1 (0 not 2 much to do 0)  
    2 ((I do not like that there is not much to do in Rochester \.) (not-like-rochester)) (0 :gist)
  1 (0 nothing 2 to do 0)  
    2 ((I do not like that there is not much to do in Rochester \.) (not-like-rochester)) (0 :gist)
  1 (0 weather 0)
    2 ((I do not like the weather in Rochester \.) (not-like-rochester)) (0 :gist)    
  1 (0 safety 0) 
    2 ((I do not like that some places are not safe in Rochester \.) (not-like-rochester)) (0 :gist)
  1 (0 transportation 0) 
    2 ((I do not like the transportation in Rochester \.)  (not-like-rochester)) (0 :gist)
  1 (0 anything 0) 
    2 ((There is nothing that I do not like in Rochester \.)  (not-like-rochester)) (0 :gist)
  1 (0 old 0) 
    2 ((I do not like old places in Rochester \.)  (not-like-rochester)) (0 :gist)
  1 (0 not 1 really 0)  
    2 ((no opinion about what I do not like in Rochester \.)  (not-like-rochester)) (0 :gist)	  
  1 (0 I wish 1 the 1 0) 
    2	((I do not like 4 in Rochester \.) (not-like-rochester)) (0 :gist)
  1 (0 like everything 0) 
    2 ((There is nothing that I do not like in Rochester \.) (not-like-rochester)) (0 :gist)
  1 (0 everything 2 nice 0) 
    2 ((There is nothing that I do not like in Rochester \.) (not-like-rochester)) (0 :gist)  
      
  1 (0) 
    2 ((NIL Gist \: nothing found for what I do not like in Rochester \.) (not-like-rochester)) (0 :gist)
))

 
(READRULES '*reaction-to-not-like-about-rochester-input*  
'(
  1 (0 not like 0 not much to do 0)  
    2 (That bothers you too\.) (100 :out)
  1 (0 weather 0) 
    2 (Yeah\, you find it cold here too\.) (100 :out)
  1 (0 not safe 0) 
    2 (Yeah \, you also find it bothering\.) (100 :out)
  1 (0 transportation 0) 
    2 (Yeah\, you wish that there were more frequent buses in the city\.) (100 :out)
  1 (0 anything 0) 
    2 (It seems I am kind of content\.) (100 :out)
  1 (0 old 0) 
    2 (Right\, you have the same feeling\.) (100 :out)
  1 (0 no opinion 0)  
    2 (That is great that I like living here\. You just wish it was a little warmer during the winter\.) (100 :out)	  

  1 (0 NIL Gist 0)
    2 (You think Rochester is generally a nice place to live \!) (100 :out)	
)); end of *reaction-to-not-like-about-rochester-input*
