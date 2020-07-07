;; *teach-BW-concept-to*: development version 7   [modified from Ben's June 5/20 version 6]
;;
;; Dialogue for blocks world concept tutoring 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defparameter *teach-BW-concept-to*

'(Event-schema ((^me teach-BW-concept-to.v ^you) ** ?e)
;```````````````````````````````````````````````````````````
; Blocks world concept tutoring session; such a session is expected to consist
; of the agent selecting a concept for which it has a concept schema, guiding 
; the user in the construction of instances of this concept, providing corrective
; feedback, and terminating when the user appears to understand the concept
; (or chooses to end the interaction). Some "small talk" may occur (currently
; disabled).
;
; [Re indexicals ^me, ^you, ^now, ... Original suggestion was @me, @you, @now, ...
; but his leads to unpleasant things like (@me teach-BW-concept-to.v @you) @ @now)
; where '@' is the EL operator for "true at the time of"]

:types (
  !t1 (^you person.n)
  !t2 (^me robot.n)  
  !t3 (?cc (plur BW-concept-structure.n)); concepts that ^me knows (as obj-schema)
  !t4 (|Table| table.n)
  !t5  ((the.d (|Target| block.n)) block.n)
  !t6  ((the.d (|Starbucks| block.n)) block.n)
  !t7  ((the.d (|Twitter| block.n)) block.n)
  !t8  ((the.d (|Texaco| block.n)) block.n)
  !t9  ((the.d (|McDonald's| block.n)) block.n)
  !t10 ((the.d (|Mercedes| block.n)) block.n)
  !t11 ((the.d (|Toyota| block.n)) block.n)
  !t12 ((the.d (|Burger King| block.n)) block.n)
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
  ; Currently not handled by Eta
  ?s1 (^me understand.v ?cc); assume that understanding a set of things
                            ; entails understanding each of them
  ?s2 (^you at-loc.p |Table|)
  ?s3 (^me at-loc.p |Table|)
)

:preconds (
  ; Currently not handled by Eta
  ?p1 (some ?c ((?c member-of.p ?cc) and (not (^you understand.v ?c))))
)

:goals (
  ; Currently not handled by Eta
  ?g1 (^me want1.v (that (^you understand1.v ?c)))
  ?g2 (^you want1.v (that ((^me teach-BW-concept-to.v ^you) @ ^now)))
)


:episodes (

  ; David introduces himself.
  ?e1 (^me say-to.v ^you 
        '(Hi\, my name is David\. I\'m ready to teach you some spatial concept \.))

  ; TODO: it might make more sense for this action to be "(^me try.v (to (choose.v ...)))",
  ; since something may go wrong - for instance, you might already understand all the concepts
  ; that Eta has available.
  ;; ?e1 (^me choose.v (a.d ?c ((most.mod-a simple.a)
  ;;       (:l (?x) (and (?x member-of.p ?cc) (not (^you understand.v ?x)))))))
  ?e2 (^me choose.v (a.d ?c (random.a
        (:l (?x) (and (?x member-of.p ?cc) (not (^you understand.v ?x)))))))

  ; Eta announces the name of the chosen concept to the user.
  ?e3 (^me say-to.v ^you '(I would like to teach you the concept of (concept-name.f ?c) \.))

  ; Eta forms (through querying the BW system) the goal representation for the
  ; simplest possible example of the concept.
  ; TODO: likewise - Eta might not have any visual/BW concepts, the concepts may be ill-formed, etc.
  ?e4 (^me form-spatial-representation.v (a.d ?goal-rep
        (:l (?x) (and (?x goal-schema1.n) (?x instance-of.p ?c)))))

  ; Eta guides the user through construction of the simple example.
  ?e5 (^me guide-BW-construction.v ^you ?goal-rep)

  ; Here, Eta assumes that the user now understands the concept after seeing them follow
  ; the instructions to build a simple example. However, in the future Eta needs to allow
  ; (or prompt) the user to choose a more complex example, and guide them through making
  ; it in a more "hands-off" way.
  ?e6 (^me say-to.v ^you '(Excellent\. You now understand the concept of (concept-name.f ?c) \.))

  ; Eta commits to memory that the user understands the concept.
  ; TODO: this needs to be stored in persistent long-term memory eventually,
  ; not just short-term context.
  ?e7 (^me commit-to-STM.v (that (^you understand.v ?c)))

  ; David says goodbye after conversation is over.
  ?e100 (^me say-to.v ^you '(Goodbye for now!))


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


)) ; END defparameter *teach-BW-concept-to*



;````````````````````````````````````````````````````````
; Store schema variable name under header in *schemas*
;
(store-schema-name 'teach-BW-concept-to.v
                  '*teach-BW-concept-to*)



;````````````````````````````````````````````````````````
; Create empty hash tables for semantics,
; gist-clauses, and topic-keys
;
(setf (get '*teach-BW-concept-to* 'semantics) (make-hash-table))
(setf (get '*teach-BW-concept-to* 'gist-clauses) (make-hash-table))
(setf (get '*teach-BW-concept-to* 'topic-keys) (make-hash-table))



;````````````````````````````````````````````````````````
; EL Semantics - Not yet used
;
(mapcar #'(lambda (x)
      (store-output-semantics (first x) (second x) '*teach-BW-concept-to*))
  '()
) ; END mapcar #'store-output-semantics



;````````````````````````````````````````````````````````
; Gist clauses
;
(mapcar #'(lambda (x) 
      (store-output-gist-clauses (first x) (second x) '*teach-BW-concept-to*))
  '()
) ; END mapcar #'store-output-gist-clauses



;````````````````````````````````````````````````````````
; Topic keys
;
(mapcar #'(lambda (x) 
      (store-topic-keys (first x) (second x) '*teach-BW-concept-to*))
  '()
) ; END mapcar #'store-topic-keys
