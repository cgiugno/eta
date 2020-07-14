(mapc 'attachfeat
  '(
    (some a)
    (put putting)
    (move moving)
  )
)

(READRULES '*output-for-correction-tree*
'(
  1 (the next step be 0 \.)
    2 (0 move 6 back 0 \.)
      3 nil (0 :out) ; "undo" actions are to be suppressed, since undoing a
                     ; move doesn't necessarily mean putting it back exactly
                     ; where it was before.
    2 (0 put 0 \.)
      3 (put some block 0 \.)
        4 (Not quite\. I need to put a block 5 \.) (4 :out)
        4 (Well\, no\, there needs to be a block 5 \.) (5 :out)
        4 (Some block should be 5 \.) (7 :out)
        4 (That doesn\'t seem to be right\. A block should be 5 \.) (4 :out)
        4 (A block needs to be 5 \.) (0 :out)
      3 (0 put 0 \.)
        4 (Not quite\. I need to put 3 \.) (4 :out)
        4 (Well\, no\, I have to place 3 \.) (5 :out)
        4 (I should put 3 \.) (7 :out)
        4 (That doesn\'t seem to be right\. I should put 3 \.) (4 :out)
        4 (I need to put 3 \.) (0 :out)
))
