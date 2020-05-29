
; TEST GOAL SCHEMA THAT CONTAIN AN EQUALITY CONSTRAINT AND AN INEQUALITY CONSTRAINT
; `````````````````````````````````````````````````````````````````````````````````

(load "planning-via-schemas.lisp")

(defparameter *kb-ht* (make-hash-table :test #'equal))

(setq *init-facts* '((on B1 B2) (on B1 B3) (on B4 B5) (on B2 table) (on B3 table)
   (on B5 B6) (on B6 table) (block b1) (block b2) (block b3) (block b4) (block b5)
   (block b6) (red b1) (red b2) (blue b3) (blue b4) (red b5) (red b6)
   (between B3 B2 B6) (between B3 B1 B6) (between B1 B2 B3) (between B5 B4 B6) ))
 
(store-facts *init-facts* *kb-ht*)

(mark-objects-with-load *kb-ht*)

(declare-as-available '(table B1 B2 B3 B4 B5 B6) '*init-facts* *kb-ht*)

(setq *goal-schema2*
; Currently only types are assumed to be in infix form. Note that 'req-properties'
; reverses infixed type predications to get prefix form.
'(rel-schema (red-and-blue-arch ?s)
  "for testing 'find-a-block-to-place' and 'place-block'"
  :vars ?x ?y ?z ?u ?v ; a list of vars for exactly the objects to be placed
  :types (?x block) (?y block) (?z block) (?u block) (?v block) (?w block)
  :nonfluent-conds (red ?x) (red ?y) (eq ?y B2) (red ?z) (red ?u) (blue ?v)
                   (not (eq ?v B4)); force choice of B3 as top block
  :static-conds
    (on ?x ?y) (on ?y table) (on ?z ?u) (on ?u table)
    (on ?v ?x) (on ?v ?z)
  :end))

(setq goal-schema-inst (copy-schema *goal-schema2*))

; Iterative object selections and placements start here (tested -- correct)

(setq obj+bind+supports (find-a-block-to-place goal-schema-inst *kb-ht*))

(place-block (car obj+bind+supports)
             (second obj+bind+supports)  goal-schema-inst *kb-ht*)

(defun f (x) (format t "~%~s" x) '-----------------------)

(f goal-schema-inst)

(setq obj+bind+supports (find-a-block-to-place goal-schema-inst *kb-ht*))

(place-block (car obj+bind+supports)
             (second obj+bind+supports)  goal-schema-inst *kb-ht*)

(setq obj+bind+supports (find-a-block-to-place goal-schema-inst *kb-ht*))

(place-block (car obj+bind+supports)
             (second obj+bind+supports)  goal-schema-inst *kb-ht*)

(setq obj+bind+supports (find-a-block-to-place goal-schema-inst *kb-ht*))

(place-block (car obj+bind+supports)
             (second obj+bind+supports)  goal-schema-inst *kb-ht*)

(setq obj+bind+supports (find-a-block-to-place goal-schema-inst *kb-ht*))

(place-block (car obj+bind+supports)
             (second obj+bind+supports)  goal-schema-inst *kb-ht*)

(get-matching-facts '(on ?x ?y) *kb-ht*)

(f (get-matching-facts '(in-use ?x)  *kb-ht*)

(mark-objects-with-load *kb-ht*)

(get 'B2 'load) ; etc.
