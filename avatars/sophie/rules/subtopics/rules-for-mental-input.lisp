(MAPC 'ATTACHFEAT
'(
 (feel feeling)
 (depressed unhappy sad down)
 (problem problems issue issues hard)
 (suicide self-harm self-hurt self-kill)
 (think thought)
))


(READRULES '*mental-input*
'(
    1 (0 feel 0)
        2 (*mental-question* (how are you feeling ?)) (0 :subtree+clause)
    1 (0)
        2 *general-input* (0 :subtree)
))


(READRULES '*mental-question*
'(
    1 (0 be you 2 depressed)
        2 ((Are you depressed ?) (mental)) (0 :gist)
    1 (0 how 1 you 1 feel 0)
        2 ((How are you feeling ?) (mental)) (0 :gist)
    1 (0 you 2 think of suicide 0)
        2 ((Have you thought of suicide ?) (mental)) (0 :gist)
    1 (0 you 2 think of self-harm 0)
        2 ((Have you thought of harming yourself ?) (mental)) (0 :gist)
    1 (0 you 2 think of self-hurt 0)
        2 ((Have you thought of hurting yourself ?) (mental)) (0 :gist)
))


(READRULES '*mental-reaction*
'(

))