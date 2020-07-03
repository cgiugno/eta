 
;; *instruct-structure-building*
;;
;; Dialogue for blocks world structure building instruction
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defparameter *instruct-structure-building*

'(Event-schema ((^me instruct-structure-building.v ^you ?goal-rep) ** ?e)
;`````````````````````````````````````````````````````````````````````````````
;
; Blocks world structure building instruction; such a session is expected to
; consist of the agent (given a goal representation) instructing the user on
; the steps required to create that goal representation, giving corrections
; to the user when they make an incorrect move, possibly answering user
; questions along the way, and terminating once the goal representation
; is successfully created.
;
; [Re indexicals ^me, ^you, ^now, ... Original suggestion was @me, @you, @now, ...
; but his leads to unpleasant things like (@me teach-BW-concept-to.v @you) @ @now)
; where '@' is the EL operator for "true at the time of"]
; Changed it to ^me, ^you, and ~now because '^' doesn't play nicely with TTT. -B.K.

:types
  !t1 (^you person.n)
  !t2 (^me robot.n)  
  !t3 (|Table| table.n)
  !t4  ((the.d (|Target| block.n)) block.n) ;; LKS: I wouldn't admit ULFs - just EL;
                                            ;; it's ok only as 2nd-order logic. 
                                            ;; In general this would require lambda-
                                            ;; abstraction in simple sentences such 
                                            ;; as "The cat at the door is hungry or
                                            ;; thirsty", plus cross-categorial
                                            ;; and/or/not, and much else 
  !t5  ((the.d (|Starbucks| block.n)) block.n)
  !t6  ((the.d (|Twitter| block.n)) block.n)
  !t7  ((the.d (|Texaco| block.n)) block.n)
  !t8  ((the.d (|McDonald's| block.n)) block.n)
  !t9  ((the.d (|Mercedes| block.n)) block.n)
  !t10 ((the.d (|Toyota| block.n)) block.n)
  !t11 ((the.d (|Burger King| block.n)) block.n)

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
;  !r9 (?ka1 (kind1-of.n action1.n))

:static-conds 
  ?s2 (^you at-loc.p |Table|)
  ?s3 (^me at-loc.p |Table|)

:preconds
  ; Currently not handled by Eta

:goals
  ; Currently not handled by Eta


:episodes 

  ; David starts conversation. It would be nice to gave opening greetings
  ; if the user is new, or it's a new day ... The opening could be more concise
  ; for repeat users.
  ?e1 (^me say-to.v ^you 
       '(OK\, let\'s start building\.))

  ?e2 (:repeat-until (?e2 finished2.a)

    ; David attempts to find next step (an action type) to realize goal structure.
    ; NOTE: ?ka1 becomes bound to a reified action corresponding to the planner
    ; output; if this is done successfully, ((pair ^me ?e3) successful.a) is 
    ; stored in context.
    ; ====== TO BE IMPLEMENTED ======
    ; try.v should take some (reified) action corresponding to a schema which ca
    ; possibly fail (in this case, finding something), and store ((pair ^me ?e3)
    ; successful.a) if the action succeeds, or ((pair ^me ?e3) unsuccessful.a)
    ; otherwise (or possibly just nothing, given that we're assuming absence-as-
    ; negation for the context.
    ; In this case, find.v should find some entity (e.g., (ka (place.v ...))))
    ; such that the entity is a step-toward the goal representation. This is to
    ; be done by querying the BW system (which is running the planner) with the
    ; goal schema for the next move. The planner output will be a spatial relation
    ; like ((the.d (|Twitter| block.n)) on.p (the.d (|Texaco| block.n))); Eta just
    ; needs to turn this into a proper reified action, i.e.,
    ; (ka (place.v (the.d (|Twitter| block.n)) (on.p (the.d (|Texaco| block.n))))).
    ; Or, in the special case of the BW system returning nil, (ka (do2.v nothing.pro)).
    ?e3 (^me try1.v (to (find4.v (some ?ka1 (?ka1 step1-toward.n ?goal-rep)))))

    ; Either (4a) next step found successfully, or (4b) failure to do so.
    ?e4 (:cond

      ; (4a)
      (((pair ^me ?e3) successful.a)

        ; Either (5a) goal structure has been reached, or (5b) goal structure
        ; not yet reached.
        ?e5 (:cond

          ; (5a)
          ((?ka1 = (ka (do2.v nothing.pro)))

            ; Next step is to do nothing; goal structure realized.
            ?e6 (^me say-to.v ^you '(The goal structure is complete\.))

            ; Terminate conversation.
            ?e7 (^me commit-to-STM.v (that (?e2 finished2.a))))
                ; LKS:``````````` I'm wondering if we can get away with a less
                ;                 technical term, like 'register.v'. Somehow
                ;                 'commit-to-STM.v' seems like an implementation
                ;                 function; a related point to ponder is whether
                ;                 checking a loop condition or branch condition
                ;                 in principle should allow for inference; e.g.,
                ;                 if we used (?e2 done.a/ended.a/concluded.a ...)
                ;                 here, would the loop still terminate, in an
                ;                 "ultimate" version of schema syntax/semantics?
                ;                 If not, it seems we might as well use
                ;                   (^me register.v '(?e2 finished2.a))
                ;                 and in loop/branch tests always check whether
                ;                 a specific formula has been registered, not
                ;                 whether it is (currently) true. Mind you,
                ;                 this would entail that a condition like
                ;                   ((^me tired.a) or (^me bored.a))
                ;                 would have to be interpreted as a sort of
                ;                 syntactic 'or' (one or the other *wff* is
                ;                 in STM); and a test like
                ;                   (?i = ?N)
                ;                 would also depend on explicit equality 
                ;                 storage, or be handled specially (probably
                ;                 the latter). I guess I lean towards the semantic
                ;                 option. So (ultimately) mental actions like
                ;                   (^me register.v (that (?e2 finished2.a))),
                ;                   (^me note2.v (that (?e2 finished2.a))),
                ;                   (^me recall.v (whether (?e2 finished2.a))),
                ;                   etc.,
                ;                 would be implemented by certain fixed storage
                ;                 or retrieval actions, but we allow some flex
                ;                 in the language that leads to those actions
                ;                 (Word embeddings might come into the picture,
                ;                 though that has pros and cons.)
                ;                     

          ; (5b)
          (:default

            ; David proposes the next step to the user.
            ; ====== TO BE IMPLEMENTED ======
            ; It seems like this ought to be fairly straightforward. Eta embeds the
            ; reified action ?ka1 in a ULF (might be good to allow multiple phrasings,
            ; to allow for the diversity of the example dialogue) such as (you.pro
            ; ((pres should.aux-s) ...)), and then converts the ULF to surface English
            ; using Gene's ulf2english lib.
            ?e8 (^me propose1-to.v ^you ?ka1)

            ; The system should not move on until the user correctly follows the action
            ; proposed in ?e8.
            ?e9 (:repeat-until (^you follow.v ?e8)
                              ; LKS: I guess that's 'follow5.v' in WordNet, i.e.,
                              ;      comply with. An alternative, focused on 
                              ;      ?ka1, would be (^you do2.v ?ka1).
                              ;        A significant question here is how we
                              ;      interpret an episodic sentence like the
                              ;      above in STM when checking a condition.
                              ;      E.g., does (^you eat.v (k lunch.n)) in STM
                              ;      mean you're eating lunch, or you *have*
                              ;      eaten lunch? Assuming that it means the
                              ;      latter (as you in effect do) than we'll
                              ;      need, say, (prog (^you eat.v (k lunch.n)))
                              ;      for an action being in progress. Or else, we
                              ;      could use (perf ((^you eat.v (k lunch.n))))
                              ;      for the completed action, while without
                              ;      modifier, "in progress" is implied. I guess
                              ;      I prefer the former.
                              ;
                              ; Other than that, this might be too coercive,
                              ; or optimistic. What if the naive user just
                              ; doesn't understand "one block straddling two
                              ; others" & the system doesn't succeed in conveying 
                              ; this notion? Then we're stuck. Or maybe the user
                              ; just expresses a desire to learn some other
                              ; concept, or to quit, or whatver. Should we rely
                              ; on interrupts? Or maybe use alternative condition
                              ;   (set-of (^you me) decide-against.v ^you ?ka1)
                              ; (we decide against you doing ?ka1)?
                              ;   I think we'd have to regard such as condition
                              ; as *implied* by (automatically inferred from)
                              ; others, from some set of alternatives. I touched
                              ; on the possible need to infer truth of tests
                              ; above, and I think this will keep coming up.
                              ; In fact, I argue in my "Computational Linguistics"
                              ; survey that many aspects of a shared context
                              ; (the "common ground") are obtained by forward
                              ; inference, e.g., beliefs are inferred from what
                              ; both can see, and what each of them says.

                       
              ; User says something in response to the system's proposal and/or
              ; makes a move.
              ; ====== TO BE IMPLEMENTED ======
              ; The following two episodes are tricky. The user can do various actions
              ; at this point, ranging from (a) moving a block to the correct location,
              ; (b) moving the block to an incorrect location, (c) asking some sort of
              ; spatial question or clarification (where we might want to make use of the
              ; spatial factors, as in the explanation questions), or even (d) some complex
              ; combination of the above, e.g. asking "here?" as they move a block to some
              ; location.
              ; Since the user may do two different types of primitive actions here - saying
              ; and moving - I've tentatively used two consecutive episodes, one where Eta
              ; observes the user doing some move action, and one where Eta observes the
              ; user saying something. However, both of these should allow for the possibility
              ; where the user doesn't do anything - i.e., the user may move a block without
              ; saying anything, or the user may ask a question without moving a block. I'm
              ; currently imagining the system waiting for up to a fixed amount of time while
              ; trying to 'fill' both of these, but the tricky part is they need to be observed
              ; simultaneously - otherwise, the user may say something while the system is waiting
              ; for a move, or vice-versa.
              ; With respect to observing moves, it seems like the BW system should simply tell
              ; Eta whether the move was judged to be correct (within some bounds) or not. If the
              ; former, ?ka2 is given the value of ?ka1. Otherwise, ?ka2 can be given a value
              ; like (ka (not move.v ... (on.p ...))), or if needed, the BW system can send one
              ; of the spatial relations which became true after the user's move and send that
              ; to Eta, giving a value like (ka (move.v ... (near.p ...))).
              ?e10 (^you acknowledge.v ?e8) ; acknowledge.v might not be the right
                                            ;  predicate here, since it might 
                                            ; include the user asking a question.
                                            ; LKS: or even saying goodbye
              ?e11 (^you perform.v ?ka2)
                   ; LKS: same as 'do.v ?ka1' (something to keep in mind);
                   ; However, you are kind of assuming that ?e10 is an acceptance,
                   ; so that one can understand ?e11 as an attempt to do ?ka1.
                   ; Yet later ?e10 ad ?e11 turn out to allow very different
                   ; things. I think we need a (try-in-seq ...) (i.e., cond)
                   ; subsuming ?e10, ?e11 and other possibilities, i.e.,
                   ; embedding some of the things that may happen (keeping 
                   ; in mind they're not in general commands!). So the first
                   ; possibility might be (^you acknowledge.v ?e8), understood
                   ; as your *accepting* the request, followed by 
                   ;   ?e11 (^you try.v ?ka1).
                   ; Now we can test whether *trying* to do ?ka1 (which is a
                   ; specific action, instantiating (ka (try.v ?ka1))) actually
                   ; *instantiated* ?ka1:
                   ;   (:if ((pair ^you ?e11) instance-of.n ?ka1)
                   ;   ; all good, on to the next action ...
                   ; Then you want those other alternatives that you get to below

              ; Either (12a) user moves block to correct location, (12b) no move was observed
              ; within the fixed time, or (12c) user moves block to incorrect location.
              ?e12 (:cond
              
                ; (12a)
                ((?ka1 = ?ka2)
                ; LKS: Better: (?ka2 subtype-of.n ?ka1), which allows equality,
                ;      synonymy, and specializations (e.g., dropping a block on
                ;      the table as a way of putting it on the table). Better yet:
                ;      ((pair ^you ?e11) instance of ?ka1) -- that's what matters.

                  ; If user's move is the same as the proposed move, store in context that 
                  ; the user followed David's proposal.
                  ?e13 (^me commit-to-STM.v (that (^you follow.v ?e8))))

                ; (12b)
                ((?ka2 = (ka (do2.v nothing.pro)))

                  ; Either (14a) the user makes a termination request, (14b) the user makes
                  ; a pause request, (14c) doesn't say anything within the fixed time, or
                  ; (14d) the user asks some question.
                  ?e14 (:cond

                    ; (14a)
                    (((ulf-of.f ?e10) = '(GOODBYE.GR))
                    ; LKS: I worry about technical jargon, really referring
                    ;      to internal representations, appearing in schemas
                    ;      (e.g., that probably at odds with learnability)
                    ;      Let's use something like 
                    ;       (:if (^you say-bye.v) ...)
                    
                      ; If user terminates conversation prematurely, store that conversation finished.
                      ?e15 (^me commit-to-STM.v (that (?e2 finished2.a))))

                    ; (14b)
                    (((ulf-of.f ?e10) = '(PAUSE.GR))
                      ; LKS: Again ...
                    
                      ; If user asks to pause, instantiate pause-conversation schema.
                      ?e16 ((set-of ^me ^you) pause-conversation.v))

                    ; (14c)
                    (((ulf-of.f ?e10) = '()) ; not sure I like this syntax...
                      ; LKS: Me neither; it should be a say-nothing condition
                    
                      ; Currently the system does nothing if it didn't detect any user speech
                      ; within the fixed time, simply causing the loop to repeat. However, we
                      ; may want to allow for the opportunity of the system trying to give a
                      ; clarification if the user is silent for an extended period of time.
                      )

                    ; (14d)
                    (:default
                    
                      ; If user asks a question David should react to them by answering the
                      ; question (i.e., instantiating the correct question-answering schema).
                      ?e17 (^me react-to.v ?e10))))

                ; (12c)
                (:default
                
                  ; David issues correction to the user.
                  ; ====== TO BE IMPLEMENTED ======
                  ; A rather un-realistic approach would simply consist of David re-asserting
                  ; ?ka1. However, like in the example dialogue, it's natural to add modifiers
                  ; (e.g., "*directly* next to the Twitter block"), or clarifications relative
                  ; to the position where the user ended up moving the block (e.g., "okay,
                  ; now move it a little bit to the left").
                  ; The former of these seems achievable by having the modifiers such as "directly"
                  ; included in the relations that the BW sends Eta in the first place, but
                  ; these modifiers are dropped (as a pragmatic principle) during the propose1-to.v
                  ; action. When the action is re-asserted as a correction, the full un-ambiguous
                  ; action including modifiers is spoken.
                  ; The latter of these seems more difficult to me - particularly since the BW
                  ; system heretofore hasn't had to communicate such concepts as "move block A
                  ; to the left by X units" to Eta - only spatial reations which can be converted
                  ; to moves, as described above.
                  ?e18 (^me issue-correction-to.v ^you ?ka1)))))))

      ; (4b)
      (:default

        ; Failure to find next step; goal structure not possible.
        ?e18 (^me say-to.v ^you
                '(I\'m afraid the example structure I had in mind cannot
                  be built with the blocks currently on the table\.))
                  ; "Let me select another example ..."?

        ; For now, the dialogue just terminates in the case of a failure at
        ; any stage in the session. However, it seems like David should select
        ; a new example of the concept if this is the case. Actually, there
        ; should probably be another constraint on the concepts/goal-schemas
        ; selected such that examples can be built with the available blocks
        ; in the first place.
        ?e19 (^me commit-to-STM.v (that (?e2 finished2.a))))))

  ; David says goodbye after conversation is over.
  ?e20 (^me say-to.v ^you '(Goodbye for now!))


:event-relations
  ; Not included yet


:necessities
  ; Not included yet


:certainties
  ; Not included yet


)) ; END defparameter *instruct-structure-building*



;````````````````````````````````````````````````````````
; Create empty hash tables for semantics,
; gist-clauses, and topic-keys
;
(setf (get '*instruct-structure-building* 'semantics) (make-hash-table))
(setf (get '*instruct-structure-building* 'gist-clauses) (make-hash-table))
(setf (get '*instruct-structure-building* 'topic-keys) (make-hash-table))



;````````````````````````````````````````````````````````
; EL Semantics - Not yet used
;
(mapcar #'(lambda (x)
      (store-output-semantics (first x) (second x) '*instruct-structure-building*))
  '()
) ; END mapcar #'store-output-semantics



;````````````````````````````````````````````````````````
; Gist clauses
;
(mapcar #'(lambda (x) 
      (store-output-gist-clauses (first x) (second x) '*instruct-structure-building*))
  '()
) ; END mapcar #'store-output-gist-clauses



;````````````````````````````````````````````````````````
; Topic keys
;
(mapcar #'(lambda (x) 
      (store-topic-keys (first x) (second x) '*instruct-structure-building*))
  '()
) ; END mapcar #'store-topic-keys
