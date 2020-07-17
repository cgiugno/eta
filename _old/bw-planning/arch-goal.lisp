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
;; (((the.d (|Twitter| block.n)) on.p (the.d (|Texaco| block.n))))
;; (((the.d (|Twitter| block.n)) on.p (the.d (|Texaco| block.n))) ((the.d (|Twitter| block.n)) on.p (the.d (|Target| block.n))))
;; (((the.d (|Twitter| block.n)) (directly.adv-a on.p) (the.d (|Texaco| block.n))))
;; (((some.d block.n) to_the_left_of.p (the.d (|Texaco| block.n))))
;; (((the.d (|Mercedes| block.n)) on.p (the.d (|Twitter| block.n))))
;; (undo ((the.d (|McDonald's| block.n)) to_the_right_of.p (the.d (|Target| block.n))))
;; (((the.d (|Mercedes| block.n)) on.p (the.d (|Twitter| block.n))))