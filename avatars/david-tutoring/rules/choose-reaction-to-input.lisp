(mapc 'attachfeat
  '()
)

(READRULES '*reaction-to-input*
  ; Currently we only branch to a question in the case
  ; of a question related to the getting-to-know you
  ; question(s). We want to be very cautious here since
  ; queries can take the form of questions. We do, however,
  ; need to expand this with various possible non-query
  ; questions, such as "can you answer wh-questions?".
 '(
   1 (0 wh_ 1 your name 0)
    2 *reaction-to-question* (0 :subtree)
   1 (0 aux you 1 answer 1 question 0)
    2 *reaction-to-question* (0 :subtree)
   1 (0 wh_ 1 questions 1 aux you 1 answer 0)
    2 *reaction-to-question* (0 :subtree)
   1 (0 aux you 0)
    2 *reaction-to-question* (0 :subtree)
   1 (0); by default, it's an assertion
    2 *reaction-to-assertion* (0 :subtree)
 ))

(READRULES '*reaction-to-assertion*
 '(
    1 (my name is 0)
      2 *reaction-to-name-input* (0 :subtree)
    1 (Eta 2 not understand 2 my name 0)
      2 *reaction-to-name-input* (0 :subtree)
    1 (spatial-question 0)
      2 *reaction-to-spatial-question-input* (0 :subtree)
    1 (Goodbye 0)
      2 (Oh\, thank me for participating\. Good bye for now!) (0 :out)
    1 (Pause 0)
      2 (Sure\, let you know when I want to continue\.) (0 :out)
    1 (Resume 0)
      2 (Hello again\.) (0 :out)
    1 (0)
      2 (You apologize \, you could not understand what I said \.) (0 :out)
  ))

(READRULES '*reaction-to-unexpected*
 '())