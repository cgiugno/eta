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