(setq *goal-rep*
; Currently only types are assumed to be in infix form. Note that 'req-properties'
; reverses infixed type predications to get prefix form.
'(rel-schema
  :header (red-and-blue-arch ?s)
  "for testing 'find-a-block-to-place' and 'place-block'"
  :vars (?x ?y ?z ?u ?v) ; a list of vars for exactly the objects to be placed
  :types ((?x block) (?y block) (?z block) (?u block) (?v block) (?w block))
  :nonfluent-conds ((red ?x) (red ?y) (red ?z) (red ?u) (blue ?v))
  :static-conds (
    (on ?x ?y) (on ?y table) (on ?z ?u) (on ?u table)
    (on ?v ?x) (on ?v ?z)
  )
  :end))


;; (((the.d (|Twitter| block.n)) to_the_left_of.p (the.d (|Texaco| block.n))))
;; (((the.d (|Target| block.n)) on.p (the.d (|Texaco| block.n))))
;; None

;; (((the.d (|Twitter| block.n)) in.p (the.d (construction.n area.n))))
;; (((the.d (|Twitter| block.n)) in.p (the.d (construction.n area.n))) ((the.d (|Twitter| block.n)) next_to.p (the.d (|Texaco| block.n))))
;; (((the.d (|Twitter| block.n)) on.p (the.d (|Texaco| block.n))))
;; (clarification ((the.d (|Twitter| block.n)) touching.p (the.d (|Texaco| block.n))))
;; (clarification ((the.d (|Twitter| block.n)) ((mod-a (by.p (one.d (half.a block.n)))) to_the_left.a)))
;; (clarification ((the.d |BW-stack|.n) ((mod-a (by.p (two.d (plur block.n)))) wide.a)))
;; (clarification (((the.d (|Twitter| block.n)) ((mod-a (by.p (two.d (plur block.n)))) wide.a)) ((the.d (|Twitter| block.n)) touching.p (the.d (|Texaco| block.n)))))
;; (clarification (((the.d (|Twitter| block.n)) touching.p (the.d (|Texaco| block.n))) ((the.d (|Twitter| block.n)) ((mod-a (by.p (two.d (plur block.n)))) wide.a))))
;; (((the.d (|Twitter| block.n)) on.p (the.d (|Texaco| block.n))) ((the.d (|Twitter| block.n)) on.p (the.d (|Target| block.n))))
;; (((the.d (|Twitter| block.n)) (directly.adv-a on.p) (the.d (|Texaco| block.n))))
;; (((some.d block.n) to_the_left_of.p (the.d (|Texaco| block.n))))
;; (((the.d (|Mercedes| block.n)) on.p (the.d (|Twitter| block.n))))
;; (undo ((the.d (|McDonald's| block.n)) to_the_right_of.p (the.d (|Target| block.n)))))
;; (((the.d (|Mercedes| block.n)) on.p (the.d (|Twitter| block.n))))


; "move the Twitter block one block to the left"
; (ka (make.v (the.d (|Twitter| block.n)) ((mod-a (by.p (one.d block.n))) to_the_left.a)))
; "move the Twitter block one half block to the left"
; (ka (make.v (the.d (|Twitter| block.n)) ((mod-a (by.p (one.d (half.a block.n)))) to_the_left.a)))
; "the Twitter block should be touching the Texaco block"
; (ka (make.v (the.d (|Twitter| block.n)) (touching.p (the.d (|Texaco| block.n)))))
; a chimney is only two blocks wide
; (ka (make.v (the.d |BW-chimney|.n) ((mod-a (by.p (two.d (plur block.n)))) wide.a)))