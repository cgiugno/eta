;; *guide-BW-construction*
;;
;; Dialogue for blocks world structure building instruction
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defparameter *guide-BW-construction*

'(event-schema :header ((^me guide-BW-construction.v ^you ?goal-rep) ** ?e)
;`````````````````````````````````````````````````````````````````````````````
; Blocks world structure building instruction; such a session is expected to
; consist of the agent (given a goal representation) instructing the user on
; the steps required to create that goal representation, giving corrections
; to the user when they make an incorrect move, possibly answering user
; questions along the way, and terminating once the goal representation
; is successfully created.

:types (
  !t1 (^you person.n)
  !t2 (^me robot.n)  
  !t3 (|Table| table.n)
  !t4  ((the.d (|Target| block.n)) block.n)
  !t5  ((the.d (|Starbucks| block.n)) block.n)
  !t6  ((the.d (|Twitter| block.n)) block.n)
  !t7  ((the.d (|Texaco| block.n)) block.n)
  !t8  ((the.d (|McDonald's| block.n)) block.n)
  !t9  ((the.d (|Mercedes| block.n)) block.n)
  !t10 ((the.d (|Toyota| block.n)) block.n)
  !t11 ((the.d (|Burger King| block.n)) block.n)
)

:rigid-conds (
  !r1 ((the.d (|Target| block.n)) blue.a)
  !r2 ((the.d (|Starbucks| block.n)) green.a)
  !r3 ((the.d (|Twitter| block.n)) red.a)
  !r4 ((the.d (|Texaco| block.n)) blue.a)
  !r5 ((the.d (|McDonald's| block.n)) green.a)
  !r6 ((the.d (|Mercedes| block.n)) red.a)
  !r7 ((the.d (|Toyota| block.n)) blue.a)
  !r8 ((the.d (|Burger King| block.n)) green.a)
)

:var-roles (
;  !r9 (?ka1 (kind1-of.n action1.n)); LKS: I've dropped it because I now think
                                    ; constraints on "ad-hoc variables (introduced
                                    ; existentially as some point) should be
                                    ; placed at that point of introduction
)

:static-conds (
  ?s2 (^you at-loc.p |Table|)
  ?s3 (^me at-loc.p |Table|)
)

:preconds (
  ; Currently not handled by Eta
)

:goals (
  ; Currently not handled by Eta
)


:episodes (
  
  ; David starts conversation. It would be nice to gave opening greetings
  ; if the user is new, or it's a new day ... The opening could be more concise
  ; for repeat users.
  ?e1 (^me say-to.v ^you 
       '(OK\, let\'s start building a simple example\.))

  ?e2 (:repeat-until (?e2 finished2.a)

    ; David attempts to find next step (an action type) to realize goal structure.
    ; NOTE: ?ka1 becomes bound to a reified action corresponding to the planner
    ; output; if this is done successfully, ((pair ^me ?e3) successful.a) is 
    ; stored in context.
    ; TODO: 'try1.v' action omitted for now.
    ; ?e3 (^me try1.v (to (find4.v (some.d ?ka1 (:l (?x) (?x step1-toward.p ?goal-rep))))))
    ?e3 (^me find4.v (some.d ?ka1 (:l (?x) (?x step1-toward.p ?goal-rep))))

    ; Either (4a) next step found successfully, or (4b) failure to do so.
    ?e4 (:try-in-sequence

      ; (4a)
      (:if ((pair ^me ?e3) successful.a)

        ; Either (5a) goal structure has been reached, or (5b) goal
        ; structure not yet reached.
        ?e5 (:try-in-sequence

          ; (5a)
          (:if (?ka1 = (ka (do2.v nothing.pro)))

            ; Next step is to do nothing; goal structure realized.
            ?e6 (^me say-to.v ^you '(Looks like the structure is completed\.))

            ; Terminate conversation.
            ?e7 (^me commit-to-STM.v (that (?e2 finished2.a))))

          ; (5b)
          (:else

            ; David guides the user in making the proposed action.
            ?e8 (^me guide-BW-action.v ^you ?ka1))))

      ; (4b)
      (:else

        ; Failure to find next step; goal structure not possible.
        ?e9 (^me say-to.v ^you
                '(I\'m afraid the example structure I had in mind cannot
                  be built with the blocks currently on the table\.))
                  ; "Let me select another example ..."?

        ; For now, the dialogue just terminates in the case of a failure at
        ; any stage in the session. However, it seems like David should select
        ; a new example of the concept if this is the case. Actually, there
        ; should probably be another constraint on the concepts/goal-schemas
        ; selected such that examples can be built with the available blocks
        ; in the first place.
        ?e10 (^me say-bye.v))))
)


:event-relations (
  ; Not included yet
)


:necessities (
  ; Not included yet
)


:certainties (
  ; Not included yet
)


)) ; END defparameter *guide-BW-construction*



;````````````````````````````````````````````````````````
; Store schema variable name under header in *schemas*
;
(store-schema-name 'guide-BW-construction.v
                  '*guide-BW-construction*)



;````````````````````````````````````````````````````````
; Create empty hash tables for semantics,
; gist-clauses, and topic-keys
;
(setf (get '*guide-BW-construction* 'semantics) (make-hash-table))
(setf (get '*guide-BW-construction* 'gist-clauses) (make-hash-table))
(setf (get '*guide-BW-construction* 'topic-keys) (make-hash-table))



;````````````````````````````````````````````````````````
; EL Semantics - Not yet used
;
(mapcar #'(lambda (x)
      (store-output-semantics (first x) (second x) '*guide-BW-construction*))
  '()
) ; END mapcar #'store-output-semantics



;````````````````````````````````````````````````````````
; Gist clauses
;
(mapcar #'(lambda (x) 
      (store-output-gist-clauses (first x) (second x) '*guide-BW-construction*))
  '()
) ; END mapcar #'store-output-gist-clauses



;````````````````````````````````````````````````````````
; Topic keys
;
(mapcar #'(lambda (x) 
      (store-topic-keys (first x) (second x) '*guide-BW-construction*))
  '()
) ; END mapcar #'store-topic-keys
