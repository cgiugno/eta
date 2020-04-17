(MAPC 'ATTACHFEAT
  '(
    (med-take take taking get getting use using)
    (work working help helping treat treating effective)
    (often frequent frequently much)
    (med-time every time times hour hours minute minutes day days week weeks)
  ))


(READRULES '*medicine-working-input*
'(

  ; What medicine are you taking?
  1 (4 wh_ 1 medicine-gen 0)
    2 (*medicine-question* (what medicine are you taking ?)) (0 :subtree+clause)
  
  ; The Lortab?
  1 (4 medicine-gen 8)
    2 (*medicine-question* (are you taking 2 ?)) (0 :subtree+clause)

  ; Do you want something better / stronger pain medication
  1 (0 do 1 you 3 want 3 med-better medicine-taking 0)
    2 (*medicine-question* (do you want stronger pain medication ?)) (0 :subtree+clause)
  1 (0 do 1 want 3 med-better 0)
    2 (*medicine-question* (do you want stronger pain medication ?)) (0 :subtree+clause)

  ; You should take something stronger / better pain medication
  1 (0 you 5 med-take 3 med-better 3 medicine-gen 0)
    2 ((You should take stronger pain medication \.) (medicine-request)) (0 :gist)
  1 (0 you 5 med-take 1 something 1 med-better 0)
    2 ((You should take stronger pain medication \.) (medicine-request)) (0 :gist)
  1 (0 you 5 want 3 med-better 2 medicine-gen 0)
    2 ((You should take stronger pain medication \.) (medicine-request)) (0 :gist)
  1 (0 you 5 want 1 something 1 med-better 0)
    2 ((You should take stronger pain medication \.) (medicine-request)) (0 :gist)

  ; You should take morphine
  1 (0 med-narcotic 0)
    2 ((You should take a narcotic \.) (medicine-request)) (0 :gist)

  ;
  ; So what we’ll do is we’ll start with the long acting morphine which works for 12 hours.  So I'm going to write for 15 milligrams every 8 hours so that you will get 90 pills because they only give you one month at a time.  But I want you to start taking only every 12 hours and if it is helping and you want one more extra you can take every 8 hours. And then in between for pain you can still take Vicodin. Do you have enough Vicodin?  You know, Vicodin is not ((      )).
  ;
  ; When you take it does it take care of the pain, though? Even if it is only temporarily?
  ;
  ; Well, we need to – well, the first call of order is maximizing your pain medication.
  ;
  ; Yeah. Yeah. Sometimes the thing a person has to do is you can take pain medicines but sometimes there’s other
  ; kinds of medicines that will make the pain medicines work better. Okay? So even simple drugs like ibuprofen or Naproxen.
  ; Aleve. You know, Motrin, those kinds of things. Will push those pain medicines to work better. Sometimes we use steroids.
  ; You know, cortical steroids like the athletes aren’t supposed to take. They are pretty potent anti inflammatories.
  ; And so that would be something we could think about adding into it. I guess I'd like to know why you have the pain
  ; before we get into steroids. But maybe what we can do is have you use your Vicodin. You could – and add some of these
  ; Motrin-like drugs into it. Have you ever taken those for any reason?
  ;
  ; D:	Are you having to watch the clock to see when you can take your next pill?
  ; P:	It was originally prescribed to take it every four hours.
  ; D:	Right.
  ; P:	Two pills every 4 hours. And I'm taking about two pills every 3 hours now.
  ; D:	All right. That’s – I don't want ((you to hurt)). Understand, that’s not my point.
  ; The problem with the Vicodin is that you get in trouble with the Tylenol in it. So you really can’t
  ; take it every 4 hours because of the Tylenol. So what I need to do is make sure you have something different for pain,
  ; we don't have to worry about that, okay?  So, you moved back here and you’ve got family here, right?
  ;
  ; Did you want, did they talk about putting you on a medication to keep your bones strong?
  ;
  ; And so I don't want to give you a stronger medicine. You’re not gonna get addicted. What you can get is called a tolerance. 
  ; A tolerance means that over time the pain receptors get lazy and they need more medicine to affect them. It’s similar to coffee.
  ; Over time coffee, if you drink one cup a day, it doesn’t start working making you awake. So you go to two cups a day.
  ; That’s not an addiction. You develop a tolerance. You can stop coffee. You’re not gonna go crazy from it. But you may
  ; need more over time. The same thing with this class of drugs, that 5 milligrams is a narcotic type of pain medicine. 
  ; It’s just a different class, right?  We’ve only got a few different classes of pain medicine. That narcotic over time?
  ; That five milligrams isn’t gonna work.  After a few months you may need a higher dose. 10 milligrams, 15 milligrams. 
  ; Something like that.  And you have real, significant pain. So my recommendation is you take a stronger medicine and that
  ; there is no ceiling on the dose of narcotic, whether it’s 15 milligrams or 100 milligrams. It doesn’t matter as long as your
  ; pain is controlled and the symptoms are reasonable, that you are not getting too sleepy or something from it. 
  ; But it’s safer than the Tylenol that you’re taking. If you take too much Tylenol it can hurt your liver.
  ;
  ; Make medication more effective
  ;

  1 (0)
    2 *general-input* (0 :subtree)
  1 (0)
    2 ((NIL Gist \: nothing found for knowing if pain medication working \.)) (0 :gist)
))


(READRULES '*medicine-working-question*
'(
))


(READRULES '*medicine-working-reaction*
'(
  1 (You should take med-narcotic \.)
    2 (*have-subdialogue* ((Usually when I hear about narcotics\, it\'s people getting addicted to them\. Is that a possibility here?)
                           ((Can I get addicted to narcotics ?)))) (100 :schema+args)
    2 (You think having the stronger pain medication would help\.) (0 :out)
  1 (You should take stronger pain medication \.)
    2 (*have-subdialogue* ((Yeah\, I think I should take a stronger pain medication\. The current one isn\'t working well\.
                            What are the side effects?)
                           ((I want a stronger pain medication \.) (What are the side effects of stronger pain medication ?)))) (100 :schema+args)
    2 (You think having the stronger pain medication would help\.) (0 :out)
  1 (0)
    2 (You think for now I\'ll wait to see if the Lortab starts helping more\.) (0 :out)
))