(READRULES '*gist-clause-trees-for-input*
   '(
      1 (what do you want to talk about ?)
         2 (*specific-answer-from-topic-input*
            *question-from-topic-input*
            nil
            nil) (0 :subtrees)
      1 (what food do you like ?)
         2 (*specific-answer-from-food-input*
            *question-from-food-input*
            nil
            nil) (0 :subtrees)
      1 (what do you think about politics ?)
         2 (*specific-answer-from-politics-input*
            *question-from-politics-input*
            nil
            nil) (0 :subtrees)
))