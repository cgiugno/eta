(mapc 'attachfeat
  '(
    (some a)
    (put putting)
    (move moving)
  )
)

(READRULES '*output-for-proposal-tree*
'(
  1 (the next step be 0 \.)
    2 (0 move 6 back 0 \.)
      3 (Not quite \. Can I put 3 back 5 ?) (4 :out)
      3 (Move 3 back 5 \.) (4 :out)
      3 (I need to put 3 back 5 \.) (4 :out)
      3 (Move 3 back 5 \.) (0 :out)
    2 (0 put 0 \.)
      3 (put some block 0 \.)
        4 (Put a block 5 \.) (4 :out)
        4 (Good\. Now place another block 5 \.) (5 :out)
        4 (Very good\. Now put another block 5 \.) (7 :out)
        4 (Next\, put a block 5 \.) (4 :out)
        4 (Put a block 5 \.) (0 :out)
      3 (0 put 0 \.)
        4 (Put 3 \.) (4 :out)
        4 (Good\. Now place 3 \.) (5 :out)
        4 (Very good\. Now put 3 \.) (7 :out)
        4 (Next\, put 3 \.) (4 :out)
        4 (Put 3 \.) (0 :out)
))
