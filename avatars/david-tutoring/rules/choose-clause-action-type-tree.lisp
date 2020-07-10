(MAPC 'ATTACHFEAT
  '(
   ))


(READRULES '*clause-action-type-tree*
; TODO: It seems that the ungainly 'spatial-question' in the gist clause here can
; now be removed, since we can resolve such a gist clause to an ask-question.v action,
; adding an appropriate conditional in the tutoring/QA schemas.
  '(
    1 (spatial-question 0)
      2 ask-question.v (0 :ulf)
    1 (Goodbye 0)
      2 say-bye.v (0 :ulf)
    1 (Pause for 1 moment 0)
      2 ask-to-pause.v (0 :ulf)
))