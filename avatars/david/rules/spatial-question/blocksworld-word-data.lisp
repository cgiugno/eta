; November 25, 2019
;=================================================================
; Word data used in the context of the blocksworld dialogue.
; Factored out so as to avoid duplication in the various rule files.
;

(MAPC 'ATTACHFEAT
'(
  (corp Adidas Burger_King Esso Heineken HP McDonalds Mercedes NVidia  
        Pepsi Shell SRI SRI_International Starbucks Texaco Target Toyota
        Twitter)

  (block blocks)
  (table tables)
  (stack stacks)
  (row rows)
  (edge edges)
  (face faces)
  (plane planes)
  (line lines)
  (circle circles)
  (pile piles)
  (object objects)
  (color colors)
  (structure structures)
  (direction directions)
  (way ways)
  (other others)
  (one ones)
  (thing things)

  (turn turns)
  (stage stages)
  (step steps)
  (question questions)
  (iteration iterations)
  (move moves)
  (period periods)
  (start starts)
  (beginning beginnings)
  (while whiles)

  (under underneath supporting support)
  (close next)
  (touching face-to-face abutting against flush)
  (farthest furthest)
  (rotated angled swivelled turned)

  (touch touches touched)
  (support supports supported)
  (connect connects connected)
  (consist_of consists_of consisted_of)
  (sit sits sat)
  (adjoin adjoins adjoined)
  (flank flanks flanked)
  (face faces faced)
  (move moves moved)
  (put puts)
  (change changes changed)
  (pick_up picks_up picked_up)
  (rotate rotates rotated)
  (place places placed)

  (prep-bw on on_to under in behind near touching facing abutting between from
        below above next_to close_to near_to visible on_top_of to_the_left_of
        to_the_right_of in_front_of adjacent_to flush_with)

  (rel-adj near close touching facing adjacent flush)
  (qual-adj purple blue green yellow orange red pink gray grey
            black white brown clear visible nearby)
  (num-adj two three four five six seven eight nine ten eleven twelve many)
  (sup-adj leftmost rightmost furthest farthest nearest closest highest
           tallest nearest topmost top uppermost smallest lowest largest
           centermost shortest backmost longest fewest frontmost)
  (ord-adj first second third fourth fifth sixth seventh eighth ninth
           tenth eleventh twelfth thirteenth fourteenth fifteenth sixteenth
           seventeens eighteenth nineteenth twentieth)
  (diff-adj other different same distinct separate unique)
  (adj-bw qual-adj rel-adj num-adj sup-adj ord-adj diff-adj)
  (mod-n adj-bw corp)

  (noun-bw block table stack row edge face plane line circle pile object
        color structure left right back front direction way other one thing)

  (verb be verb-rel)
  (be is are was were)
  (verb-rel touch support connect consist_of sit adjoin flank face
      move put change pick_up rotate place)

  (aux-bw do modal)
  
  (adv-history previously before originally initially currently recently ever since always)
  (noun-history turn stage step question iteration move period start beginning while past)
  (prep-history at in on prep-history-simple)
  (prep-history-simple during before after prior_to following preceding since)
  (adj-history previous next current initial original following preceding future last final recent)

  ; Any word that signals that the following expression might be an adv-e
  ; 'right' for e.g., right before this turn
  ; number determiners for e.g., two turns ago
  (adv-e adv-history prep-history adj-history right one two three four five six seven eight nine ten)
  (adv_ adv adv-history)
  
  (prep prep-bw prep-history)
  (adj adj-bw adj-history)
  (noun noun-bw noun-history)
  (np-bw np_ noun-bw)
))