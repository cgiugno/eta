;; This was the first test file, intended for tests of various functions
;; in the block-selection code. The first schema shown was just for checking
;; whether various functions coped correctly with schema syntax. The second
;; schema is a simple goal schema specifyig a 3-block red stack.
;;
;; Functions tested included 'req-supports-of' (required supports for a goal block)
;; and 'find-a-block-to-place' (finding an object variable in a goal schema
;; that remains unbound, but its supports are in place, i.e., bound to constants,
;; and then find an available object to instantiate that variable).
;;
;; Additional helper functions that have been tested (see below) are primarily
;; 'constrain-relation', 'get-matching-facts', and 'find-all-instances'.
;;
;; That was the status 2017-18. Recently (May 2020) I added a schema for a
;; small "arch" for fuller testing

(setq schema
'(test-schema
  "for testing req-supports-of function"; w/ random extras as illustrative stuff
  :vars ?x (?y b) ?z ; here ?y is already bound to b
  :types (?x small-block) (?y big-block)
  :nonfluent-conds whatever1 (bigger-than ?y ?x) whatever2 (eq ?z c)
         (box ?z) (bigger-than ?z x) (cubical ?x) (cubical ?y) (square ?z)
         (not (attached-to ?x ?z))
  :static-conds
    (on ?x ?y) (on ?y c) (under Jo table) (on ?z table) (on c ?z) (next-to a c)
    "well, why shouldn't someone be under the table"
    (on table floor) (on b dirt) (in jo room1) (box b)
  :comment "ok, almost enough"
    (on ?y paper) (in juliet love); (k love) wouldn't be handled here
  :ordering-constraints blah blah
  :end))

(setq goal-schema
; I should really switch to infix syntax, consistently with LISSA, etc.
; Currently only types are assumed to be in infix form. Note that 'req-properties'
; reverses infixed type predications to get prefix form.
'(rel-schema (red-stack ?s)
  "for testing find-a-block-to-place"
  :vars ?x ?y ?z
  :types (?x block) (?y block) (?z block)
  :nonfluent-conds (red ?x) (red ?y) (red ?z) 
  :static-conds
    (on ?x ?y) (on ?y ?z) (on ?z table) ; block off 'loose-part-of' 5/15/2020
   ;(loose-part-of ?x ?s) (loose-part-of ?y ?s) (loose-part-of ?z ?s)
  :end))

(setq goal-schema-inst (copy-schema goal-schema))

(setq *robot-posn* '(0 0))
; Just in case we use coordinate information ultimately

; Data for testing 'constrain-relation' and 'find-a-block-to-place':
;``````````````````````````````````````````````````````````````````
; (Need to load "planning-via-schemas.lisp" first)
;
(setq kb-ht (make-hash-table :test #'equal))

(setq pred1 '((on B1 B2) (on B1 B3) (on B4 B5) (on B2 table) (on B3 table) 
              (on B5 B6) (on B6 table)))
(setq pred1-patt '(on ?x ?y))
(setq pred2 '((block b1) (block b2) (block b3) (block b4) (block b5) (block b6)))
(setq pred2-patt '(block ?y))
(setq pred3 '((red b1) (red b4) (red b5)))                             
(setq pred3-patt '(red ?x?))
(setq pred4-patt '(red ?y))
(setq pred5 '((blue b2) (blue b3) (blue b6)))
(setq pred5-patt '(blue ?y))

(setq *init-facts* '((on B1 B2) (on B1 B3) (on B4 B5) (on B2 table) (on B3 table)
   (on B5 B6) (on B6 table) (block b1) (block b2) (block b3) (block b4) (block b5) 
   (block b6) (red b1) (red b4) (red b5) (blue b2) (blue b3) (blue b6)
   (between B3 B2 B6) (between B3 B1 B6) (between B1 B2 B3) (between B5 B4 B6) ))
   ; NB: I haven't put in symmetry versions, like (between B3 B6 B2); could do so.

(store-facts *init-facts* kb-ht)

(mark-objects-with-load kb-ht)

(defun f (x) (format t "~s" x) '------------------) ; for full display of results

; Prepare for processing the goal-schema-inst
(declare-as-available '(table B1 B2 B3 B4 B5 B6) '*init-facts* kb-ht)

(trace unbound-vars-of req-supports-of req-properties find-all-instances 
   choose-easily-reached get-matching-facts constrain-relation)

(find-a-block-to-place goal-schema-inst kb-ht)
; May 15/20 -- this hadn't bee previously tried.
; ==> ((?Z B1) (TABLE))

(setq rel (constrain-relation pred1-patt pred2-patt pred1 kb-ht))
; should be ((ON B1 B2) (ON B1 B3) (ON B4 B5) (ON B5 B6)) (i.e., not on table)
(f rel)

(setq rel2 (constrain-relation pred1-patt pred3-patt pred1 kb-ht))
; should be ((on B1 B2) (on B1 B3) (on B4 B5) (on B5 B6)) (red blocks on smthg)
(f rel2)

(setq rel3 (constrain-relation pred1-patt pred4-patt pred1 kb-ht))
; should be ((on B4 B5)) (smthg on a red block)
(f rel3)

(setq rel4 (constrain-relation pred1-patt pred5-patt rel2 kb-ht));
; should be ((on B1 B2) (on B1 B3) (on B5 B6)) (red blocks on blue blocks)
(f rel4)

; Examples including negations:

(declare-as-available '(B1 B2 B3 B4 B5 B6) '*init-facts* kb-ht)(setq pred6-patt '(not (red ?y)))
(setq rel5 (constrain-relation pred1-patt pred6-patt pred1 kb-ht))
; Should be ((on B1 B2) (on B1 B3) (on B2 table) (on B3 table) (on B5 B6) (on B6 table)) (i.e., not on something red, which excludes (on B4 B5))

(setq pred8-patt '(between ?y ?x ?z))
(setq pred7 '((between B3 B2 B6) (between B3 B1 B6) (between B1 B2 B3) 
              (between B5 B4 B6)))
(setq rel6 (constrain-relation pred8-patt  pred6-patt pred7 kb-ht))
; non-red objects between two objects -- should give ((between B3 B2 B6)
; (between B3 B1 B6))

(get-matching-facts '(between ?x B2 ?y) kb-ht)
; Should be ((between B3 B2 B6) (between B1 B2 B3))

(get-matching-facts '(between ?x B2 B3) kb-ht)
; Should get ((between B1 B2 B3))

(get-matching-facts '(between B3 ?x B6) kb-ht)
; should give ((between B3 B2 B6) (between B3 B1 B6))

(find-all-instances '(:l (?x) (on ?x ?y)) kb-ht)
; Should give all 6 blocks -- they're all on something, i.e.
; (B1 B4 B2 B3 B5 B6) (in order of occurrence in *init-facts*)

(find-all-instances '(:l (?x) (on ?x table)) kb-ht)
; Should be (B2 B3 B6)

(find-all-instances '(:l (?x) (and (on ?x ?y) (block ?y))) kb-ht)
; (B1 B4 B5)

(find-all-instances '(:l (?x) (and (on ?x ?y) (red ?x) (not (red ?y)))) kb-ht)
; (B1 B5)

(find-all-instances '(:l (?x ?y) (between ?z ?x ?y)) kb-ht)
; Should give ((B2 B6) (B1 B6) (B2 B3) (B4 B6)) ; (the ordering was changed)

(find-all-instances '(:l (?x ?y) (and (between ?z ?x ?y) (red ?x) (not (red ?z))))
                    kb-ht)
; Should be ((B1 B6))

(find-all-instances '(:l (?x ?y) (and (on ?y ?x) (red ?y) (not (red ?x)))) kb-ht)
; should be ((B2 B1) (B3 B1) B6 B5))

; (Nov 22/17: correct so far)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Resuming testing in May 2020:
;;
;; Assume the initializations of the world model etc. have been done as above,
;; including 
;;    (setq goal-schema-inst (copy-schema goal-schema))
;; as well as
;;  (defun f (x) (format t "~s" x) '------------------) ; for full result display

(req-supports-of '?x goal-schema-inst)
; ==> (?Y); for ?y it gives (?Z), & for ?z it gives (TABLE)  {May 16/20 - correct}

(f (req-supports-of '?x schema))
; ==> (?Y); for argument ?y ==> (C PAPER)  {May 16/20 -- correct}

(f (req-properties '?x goal-schema-inst))
; ==> ((BLOCK ?X) (RED ?X)) {May 16/20}

; We should be able to apply this to 'schema' (the initial test schema) as well:
; (f (req-properties '?x schema)) ==>
;    ((small-block ?x) (bigger-than ?y ?x) (bigger-than ?z x) (cubical ?x)
;     (not (attached-to ?x ?z)))
; but I think the only relations that are checked are ones involving only
; already-bound variables; building something that requires taking account
; of, say, size relations *in advance* of starting construction is almost
; certainly not handled -- maybe only "by luck", if objects positioned
; earlier happen to have the right size relations to objects placed later.
; (I suppose as a crude combinatorial method one could use backtracking
; until one *does* get lucky.)
; 
; Anyway, what *should* work in all cases where the required properties 
; are monadic. 

(f (find-a-block-to-place goal-schema-inst kb-ht))
; ==> ((?Z B1) (TABLE)); B1 is one of the red blocks.
; However, I have 'loose-part-of' relations between blocks and the stack s,
; and that's not available in the world model. I've commented out those
; requirements. If I reinstate something similar, I think I better change
; from binary 'loose-part-of' to monadic 'manipulable', and put this in
; the KB. (We could use inference from 'block' to 'manipulable', but that's
; not for right now; Gridworld has inference code for that ...

;                       ---------------------------------

; Another goal schema, for a small arch:

(setq goal-schema2
; I should really switch to infix syntax, consistently with LISSA, etc.
; Currently only types are assumed to be in infix form. Note that 'req-properties'
; reverses infixed type predications to get prefix form.
'(rel-schema (red-and-blue-arch ?s)
  "for testing 'find-a-block-to-place' and 'place-block'"
  :vars ?x ?y ?z ?u ?v ?w
  :types (?x block) (?y block) (?z block) (?u block) (?v block) (?w block)
  :nonfluent-conds (red ?x) (red ?y) (red ?z) (red ?u) (blue ?v) (blue ?w)
  :static-conds
    (on ?x ?y) (on ?y table) (on ?z ?u) (on ?u table) 
    (on ?w ?x) (on ?w ?z)
   
  :end))

(setq *init-facts* '((on B1 B2) (on B1 B3) (on B4 B5) (on B2 table) (on B3 table)
   (on B5 B6) (on B6 table) (block b1) (block b2) (block b3) (block b4) (block b5)
   (block b6) (red b1) (red b2) (blue b3) (blue b4) (red b5) (red b6)
   (between B3 B2 B6) (between B3 B1 B6) (between B1 B2 B3) (between B5 B4 B6) ))

; Testing of this was continued in other test files ...
; see "test-arch-construction.lisp"
