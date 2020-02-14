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
   1 (0 ?); anything ending with ?
    2 *reaction-to-question* (0 :subtree)
   1 (0); by default, it's an assertion
    2 *reaction-to-assertion* (0 :subtree)
 ))

(READRULES '*reaction-to-assertion*
'(
  1 (0 medicine 0)
    2 *medicine-reaction* (0 :subtree)
  1 (0 years left alive 0)
    2 *prognosis-reaction* (0 :subtree)
  1 (0)
    2 *general-reaction* (0 :subtree)
))

(READRULES '*reaction-to-unexpected*
 '())