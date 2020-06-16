;; *teach-BW-concept-to*: development version 7   [modified from Ben's June 5/20 version 6]
;;
;; Dialogue for blocks world concept tutoring 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defparameter *teach-BW-concept-to*

'(Event-schema ((~me teach-BW-concept-to.v ~you) ** ?e)
;```````````````````````````````````````````````````````````
; Blocks world concept tutoring session; such a session is expected to consist
; of the agent selecting a concept for which it has a concept schema, guiding 
; the user in the construction of instances of this concept, providing corrective
; feedback, and terminating when the user appears to understand the concept
; (or chooses to end the interaction). Some "small talk" may occur (currently
; disabled).
;
; [Re indexicals ~me, ~you, ^now, ... Original suggestion was @me, @you, @now, ...
; but his leads to unpleasant things like (@me teach-BW-concept-to.v @you) @ @now)
; where '@' is the EL operator for "true at the time of"]

:types
  !t1 (~you person.n)
  !t2 (~me robot.n)  
  !t3 (?cc (plur BW-concept.n)); concepts that ~me knows (as obj-schema)
  !t4 (|Table| table.n)
  !t5  ((the.d (|Target| block.n)) block.n)
  !t6  ((the.d (|Starbucks| block.n)) block.n)
  !t7  ((the.d (|Twitter| block.n)) block.n)
  !t8  ((the.d (|Texaco| block.n)) block.n)
  !t9  ((the.d (|McDonald's| block.n)) block.n)
  !t10 ((the.d (|Mercedes| block.n)) block.n)
  !t11 ((the.d (|Toyota| block.n)) block.n)
  !t12 ((the.d (|Burger King| block.n)) block.n)

:rigid-conds
  !r1 ((the.d (|Target| block.n)) blue.a)
  !r2 ((the.d (|Starbucks| block.n)) green.a)
  !r3 ((the.d (|Twitter| block.n)) red.a)
  !r4 ((the.d (|Texaco| block.n)) blue.a)
  !r5 ((the.d (|McDonald's| block.n)) green.a)
  !r6 ((the.d (|Mercedes| block.n)) red.a)
  !r7 ((the.d (|Toyota| block.n)) blue.a)
  !r8 ((the.d (|Burger King| block.n)) green.a)

:var-roles
;  !r9 (?ka1 (kind1-of.n action1.n)); LKS: I've dropped it because I now think
                                    ; constraints on "ad-hoc variables (introduced
                                    ; existentially as some point) should be
                                    ; placed at that point of introduction

:static-conds 
  ; Currently not handled by Eta
  ?s1 (~me understand.v ?cc); assume that understanding a set of things
                            ; entails understanding each of them
  ?s2 (~you at-loc.p |Table|)
  ?s3 (~me at-loc.p |Table|)

:preconds
  ; Currently not handled by Eta
  ?p1 (some ?c ((?c member-of.n ?cc) and (not (~you understand.v ?c))))

:goals
  ; Currently not handled by Eta
  ?g1 (~me want1.v (that (~you understand1.v ?c)))
  ?g2 (~you want1.v (that ((~me teach-BW-concept-to.v ~you) @ ^now)))


:episodes 

  ; David starts conversation. It would be nice to gave opening greetings
  ; if the user is new, or it's a new day ... The opening could be more concise
  ; for repeat users.

  ?e1 (~me say-to.v ~you 
       '(OK\, Here\'s what I\'ll try to do\: I\'ll try to teach you a concept  
        that can be illustrated with the blocks in front of us\, ok?))

  ?e2 (~you reply-to.v ?e1)
  ; This would presumably be handled with a subschema or ad hoc subplan?
  ; If the reply is positive ("OK", "Go ahead", "Why not?", etc.), we should 
  ; just move on to the next step. If the user wants to terminate right there,
  ; then there should be good-byes; if the user raises a question about the
  ; process, responding "I'm about to tell you more" might be good enough.
  ;
  ; One general kind of reply that would be nice to be able to handle is
  ; "Can you repeat that?", where perhaps the repetition might use a slightly
  ; different wording ...

  ?e3 (~me say-to.v ~you
       '(So what I\'ll do is to pick a concept that I understand\, and then
         I\'ll guide you in the construction of an example of the concept \...
         Ready?))

  ?e4 (~you reply-to.v ?e3)
  ; We might ignore the reply ... It's just good to allow for acknowledgement

  ?e5 (a.d ?c ((?c member-of.n ?cc) and (not (~you understand.v ?c))) 
              (~me choose.v ?c))
  ; Assume dynamic semantics for ?c. When instantiating, it can be replaced
  ; by a concept designator (also in later occurrences of ?c), dropping the 
  ; quantifier. Example designator: (k BW-arch.n).
 
  ?e6 (~me say-to.v ~you
       '(OK\, in my mind I've picked a concept. I won\'t name it\, because that
         might bias how you interpret examples. I\'ll now try to give you 
         step-by-step instructions for building an example of the concept.))

  ?e7 (~you acknowledge.v ?e6)
  ; This should succeed even if there' no "OK", just a couple of seconds of silence

  ?e8 (a.d ?goal-rep ((?goal-rep goal-schema1.n) and (?goal-rep instance-of.n ?c))
                     (~me create.v ?goal-rep))

  ?e2 (:repeat-until (:in-context (?e2 finished2.a))
  ; LKS: I lean towards (:repeat-until (?e2 finished2.a), on the assumption 
  ; that loop termination and branching tests always check in contextual memory 
  ; (except when the test is for the value of a variable), where absence means
  ; negation (CWA). The very fact that (?e2 finished2.a) appears as a test
  ; should tell the schema executor that (?e2 finished2.a) should be put int
  ; contextual memory if it becomes true.

    ; David attempts to find next step (an action type) to realize goal structure.
    ; NOTE: ?ka1 becomes bound to a reified action corresponding to the planner
    ; output; if this is done successfully, ((pair ~me ?e3) successful.a) is 
    ; stored in context.
    ; ====== TODO ======
    ; try.v should take some (reified) action schema which can possibly fail, and
    ; store ((pair ~me ?e3) successful.a) if the action succeeds.
    ; In this case, find.v should find some entity (e.g., (ka (place.v ...))))
    ; such that the entity is a step-toward the goal representation.
    ?e3 (~me try1.v (to (find4.v (some ?ka1 (?ka1 step1-toward.n ?goal-rep)))))
    ; LKS: I've dropped the (kind1-of.n action1.n) constraint on the assumption
    ; this is entailed anyway by being a step toward a goal schema. 

    ; LKS: Everything that follows is really more concerned with collaborative 
    ; building rather than building an example of a concept, and when done,
    ; testing if the user already thinks s/he has an idea of what the concept
    ; is, etc.

    ; Either (4a) next step found successfully, or (4b) failure to do so.
    ?e4 (:if

        ; (4a)
        ((:in-context ((pair ~me ?e3) successful.a))
        ; LKS: Again, I think (:if ((pair ~me ?e3) successful.a) ...) should
        ; suffice.

          ; Either (5a) goal structure has been reached, or (5b) goal structure
          ; not yet reached.
          ?e5 (:if

            ; (5a)
            (?ka1 = (ka (do2.v nothing.pro))); tentatively changed from (:equal ...)
            ; LKS: When this test is made, ?ka1 will probably have been replaced 
            ; by a Skolem name for the kind of action -- though an expression
            ; of form (ka <pred>) is also a name for a kind of action, and we
            ; could substitute this throughout. If a Skolem name was used,
            ; then this needs to be equated to the (ka <pred>) expression.
            ; If the action-denoting expression was substituted for ?ka1
            ; directly, then the above check can probably done at the syntactic
            ; level -- i.e., are the expressions the same? Otherwise, the equality
            ; (stored in contextual memory) needs to be consulted. Interestingly,
            ; in either case "lookup" may not be enough, because there can in
            ; principle be different expressions for the same action type.
            ; (e.g., (ka (do2.v nothing-at-all.pro)) here, though this is not
            ; a likely example).

              ; Next step is to do nothing; goal structure realized.
              ?e6 (~me say-to.v ~you '(The goal structure is complete\.))

              ; Terminate conversation.
              ?e7 (:store-in-context '(?e2 finished2.a)))
              ; LKS: I'm a little bit dubious even about ':store-in-context';
              ; Maybe use ?e7 (~me commit-to-STM.v (that (?e2 finished2.a)))

            ; (5b)
            (:default

              ; David proposes the next step to the user.
              ; ====== TODO ======
              ; Should embed reified action in ?ka1 in a ULF like
              ; (you.pro ((pres should.aux-s) ...)) and then convert it to English.
              ?e8 (~me propose1-to.v ~you ?ka1)

              ; Allow for user follow-up questions until the user accepts/rejects
              ; the proposal by saying something like "yes", "okay", or "no", 
              ; or (ideally - see note below) just making a move.
              ?e9 (:repeat-until (:in-context (~you accept.v ?e8))
              
                ; User says something to David. Either acknowledgement, or 
                ; a query of some sort.
                ; NOTE: it's entirely possible that the user does not say 
                ; anything at all, and gives "tacit" acceptance or rejection
                ; of the proposal by making the proposed move, or making 
                ; a different move than the proposed one, respectively.
                ; This isn't supported in the current form of this schema,
                ; and I'm a bit at a loss as to how to "encode" these sorts 
                ; of tacit statements in the framework of this schema while 
                ; simultaneously allowing for iterated follow-up questions 
                ; by the user.
                ?e10 (~you say-to.v ~me ?ulf)

                ; Either (11a) user accepts proposal, (11b) user rejects
                ; proposal, (11c) makes a termination ; request, or (11d) asks 
                ; some query.
                ?e11 (:if

                  ; (11a)
                  ((:equal ?ulf '(YES.YN))
                  
                    ; If accepted, store that the proposal has been accepted 
                    ; in context.
                    ?e12 (:store-in-context '(~you accept.v ?e8)))

                  ; (11b)
                  ((:equal ?ulf '(NO.YN))

                    ; If rejected issue correction.
                    ; NOTE: according to this schema David is rather inflexible,
                    ; simply repeating the same proposal over and over again to 
                    ; the user until they do the right move. However, ideally 
                    ; it seems that David should try to find a new plan consistent
                    ; with the user's own proposed move, or even try to find a 
                    ; new goal structure consistent with the user's proposed 
                    ; move. Both of these would add another level of complexity
                    ; to the current schema.
                    ; ====== TODO ======
                    ?e13 (~me issue-correction-to.v ~you ?ka1))

                  ; (11c)
                  ((:equal ?ulf '(GOODBYE.GR))
                  
                    ; If user requests to terminate conversation prematurely, 
                    ; store that the outer loop is finished.
                    ?e14. (:store-in-context '(?e2 finished2.a)))

                  ; (11d)
                  (:default

                    ; If the user said something other than a clear acceptance 
                    ; or rejection of the proposed action, David should reply 
                    ; to it - instantiating the appropriate subschema such as 
                    ; reactions-to-historical-question or reactions-to-spatial-
                    ; question.
                    ?e15 (~me reply-to.v ?e10.))))))

        ; (4b)
        (:default

          ; Failure to find next step; goal structure not possible.
          ?e16 (~me say-to.v ~you
                  '(I cannot build the structure with the available blocks\.))

          ; For now, the dialogue just terminates in the case of a failure at
          ; any stage in the session. However, we might want to explore recovery
          ; options, like finding a new goal structure compatible with the 
          ; blocks at time of failure.
          ?e17 (:store-in-context '(?e2 finished2.a)))))

  ; David says goodbye after conversation is over.
  ?e18 (~me say-to.v ~you '(Goodbye for now!))


:event-relations
  ; Not included yet


:necessities
  ; Not included yet


:certainties
  ; Not included yet


)) ; END defparameter *teach-BW-concept-to*



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
