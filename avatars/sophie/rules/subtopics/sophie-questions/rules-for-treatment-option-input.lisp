(MAPC 'ATTACHFEAT
'(
))


(READRULES '*treatment-option-input*
'(

  ; So, again, I think that one of the most important things, something, you know, that’s important to understand, 
  ; that generally Stage 4 lung cancer is not something we can cure.  The chemotherapy can – the goal of the chemotherapy 
  ; is to try to slow the cancer down, to try to prevent it from spreading as much as possible. Sometimes with the chemotherapy
  ; we can see certain areas shrink. Our hope with the chemotherapy is that by slowing it down and preventing it from spreading
  ; that we can preserve your quality of life as much as possible.
  ;
  ; you know, studies and things would suggest that chemotherapy can give you more time compared to not doing anything at all,
  ; compared to not doing chemotherapy. It’s impossible for me to predict exactly how much more time you have. You know,
  ; I think people often wonder how long do I have.
  ;
  ; So let me just lay out a potential best case scenario. What a potential best case scenario may be, we do another set of scans.
  ;  We still see some active cancer areas in the bones and in the lung. We’re able to get you started on therapy sometime within
  ; the next couple of weeks. And if you tolerate treatment usually what we’ll do is we treat for four to six cycles of chemotherapy. 
  ; And then we stop treatment and we continue to monitor things with scans on a regular basis.
  ; 
  ; So I think best case scenario again would be we do six cycles of treatment, things stay stable for a while, we do scans
  ; and they stay stable. Eventually the cancer will likely grow, even in the best case scenario it will likely start to grow
  ; again. But at that time we often have second line treatments we can try. Some patients can stay on treatment for many
  ; months, even a couple of years or longer. That’s kind of a best case scenario.

  1 (0 chemotherapy 0)
    2 ((Chemotherapy is a treatment option \.) (chemotherapy)) (0 :gist)
  1 (0)
    2 *general-input* (0 :subtree)
  1 (0)
    2 ((NIL Gist \: nothing found for treatment option \.)) (0 :gist)
))


(READRULES '*treatment-option-question*
'(

))


(READRULES '*treatment-option-reaction*
'(
  1 (0)
    2 (You will have to think about what you said more\.) (0 :out)
))