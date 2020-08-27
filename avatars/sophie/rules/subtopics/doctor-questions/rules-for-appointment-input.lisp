(MAPC 'ATTACHFEAT
  '(
    (married marry spouse partner husband wife)
    (doing feeling feel)
  ))


(READRULES '*appointment-input*
'(
  ; Asking about where your daughter works
  1 (0 where does 3 work 0)
    2 (0 she 0)
      3 ((Where does your daughter work ?) (anyone-here-with-you)) (0 :gist)
    2 (0 your daughter 0)
      3 ((Where does your daughter work ?) (anyone-here-with-you)) (0 :gist)
    2 (0 he 0)
      3 ((Where does your son work ?) (anyone-here-with-you)) (0 :gist)
    2 (0 your son 0)
      3 ((Where does your son work ?) (anyone-here-with-you)) (0 :gist)
  1 (0 how old be 0)
    2 (0 she 0)
      3 ((How old is your daughter ?)) (0 :gist)
    2 (0 your daughter 0)
      3 ((How old is your daughter ?)) (0 :gist)
    2 (0 he 0)
      3 ((How old is your son ?)) (0 :gist)
    2 (0 your son 0)
      3 ((How old is your son ?)) (0 :gist)
  ;Do you have grandchildren or children?
  1 (0 children 0)
   2 ((Do you have any children ?) (anyone-here-with-you)) (0 :gist)
  1 (0 child 0)
   2 ((Do you have any children ?) (anyone-here-with-you)) (0 :gist)
  1 (0 grandchildren 0)
   2 ((Do you have any grandchildren ?) (anyone-here-with-you)) (0 :gist)
  1 (0 grandchild 0)
   2 ((Do you have any grandchildren ?) (anyone-here-with-you)) (0 :gist)
  ;Were you ever married?
  1 (0 married 0)
   2 ((Are you married ?) (anyone-here-with-you)) (0 :gist)
  ;How are you doing today?
  1 (0 wh_ 2 you 3 feeling 0)
   2 ((How are you doing today ?)) (0 :gist)

  ; Expressing sympathy about daughter not being able to make it
  1 (0 that be too bad 0)
    2 ((I am sorry that your daughter couldn\'t come today \.) (anyone-here-with-you)) (0 :gist)
  1 (0 I be sorry 0)
    2 ((I am sorry that your daughter couldn\'t come today \.) (anyone-here-with-you)) (0 :gist)
  1 (0)
    2 *general-input* (0 :subtree)
))


(READRULES '*appointment-question*
'(
  ; Did you drive here?
  1 (0 drive 0)
    2 ((Did you drive here ?)) (0 :gist)

  ; Is anyone here with you?
  1 (0 be you here 1 with 0)
    2 ((Is anyone here with you ?)) (0 :gist)
  1 (0 anyone be here 1 with you 0)
    2 ((Is anyone here with you ?)) (0 :gist)
  1 (0 who be here 1 with you 0)
    2 ((Is anyone here with you ?)) (0 :gist)
  1 (0 did you come with 0)
    2 ((Is anyone here with you ?)) (0 :gist)
  1 (0 be 1 here 1 with you 0)
    2 ((Is anyone here with you ?)) (0 :gist)
  1 (0 be you here alone 0)
    2 ((Is anyone here with you ?)) (0 :gist)
  1 (0 did you come by yourself 0)
    2 ((Is anyone here with you ?)) (0 :gist)
  1 (0 who came with you 0)
    2 ((Is anyone here with you ?)) (0 :gist)
  1 (0 come here with 0)
    2 ((Is anyone here with you ?)) (0 :gist)

   ; Asking about where your daughter works
  1 (0 where does 3 work 0)
    2 (0 she 0)
      3 ((Where does your daughter work ?) (anyone-here-with-you)) (0 :gist)
    2 (0 your daughter 0)
      3 ((Where does your daughter work ?) (anyone-here-with-you)) (0 :gist)
    2 (0 he 0)
      3 ((Where does your son work ?) (anyone-here-with-you)) (0 :gist)
    2 (0 your son 0)
      3 ((Where does your son work ?) (anyone-here-with-you)) (0 :gist)
  1 (0 how old be 0)
    2 (0 she 0)
      3 ((How old is your daughter ?)) (0 :gist)
    2 (0 your daughter 0)
      3 ((How old is your daughter ?)) (0 :gist)
    2 (0 he 0)
      3 ((How old is your son ?)) (0 :gist)
    2 (0 your son 0)
      3 ((How old is your son ?)) (0 :gist)
  ;Do you have grandchildren or children?
  1 (0 grandchildren 0)
   2 ((Do you have any grandchildren ?) (anyone-here-with-you)) (0 :gist)
  1 (0 grandchild 0)
   2 ((Do you have any grandchildren ?) (anyone-here-with-you)) (0 :gist)
  1 (0 children 0)
   2 ((Do you have any children ?) (anyone-here-with-you)) (0 :gist)
  1 (0 child 0)
   2 ((Do you have any children ?) (anyone-here-with-you)) (0 :gist)
  ;Were you ever married?
  1 (0 married 0)
   2 ((Are you married ?) (anyone-here-with-you)) (0 :gist)
  ;How are you doing today?
  1 (0 wh_ 2 you 3 feeling 0)
   2 ((How are you doing today ?)) (0 :gist)


  1 (0)
    2 ((NIL Gist)) (0 :gist)
))


(READRULES '*appointment-reaction*
'(

))
