;; *ETA-SCHEMA*: development version 6
;;
;; Dialogue for blocks world conversation 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defparameter *eta-schema*

'(Event-schema (((set-of me you) have-eta-dialog.v) ** ?e)
;```````````````````````````````````````````````````````````
; Blocks world conversation. An expected blocks world dialogue consists of the agent repeatedly asking
; the user if they have a spatial question to ask, followed by a question from the user, followed by
; an appropriate response by the agent. This repeats until the user says something interpreted as a goodbye,
; and is possibly preempted by some "smalltalk" questions (currently disabled).
;

:types
  !t1 (you person.n)
  !t2 (me robot.n)
  !t3 (?t table1.n)
  ; Eventually this should be changed to just ?bb (plur block1.n)
  !t4  (?b1 block.n)
  !t5  (?b2 block.n)
  !t6  (?b3 block.n)
  !t7  (?b4 block.n)
  !t8  (?b5 block.n)
  !t9  (?b6 block.n)
  !t10 (?b7 block.n)
  !t11 (?b8 block.n)

:rigid-conds
  !r1 ((the.d (|Target| block.n)) blue.a)
  !r2 ((the.d (|Starbucks| block.n)) green.a)
  !r3 ((the.d (|Twitter| block.n)) red.a)
  !r4 ((the.d (|Texaco| block.n)) blue.a)
  !r5 ((the.d (|McDonald's| block.n)) green.a)
  !r6 ((the.d (|Mercedes| block.n)) red.a)
  !r7 ((the.d (|Toyota| block.n)) blue.a)
  !r8 ((the.d (|Burger King| block.n)) green.a)

:fluent-conds
  !f1 (?ka1 (kind1-of.n action1.n))
  !f2 (?goal-rep goal-schema1.n)


:static-conds 
  ?s1 (you at-loc.p ?t)
  ?s2 (me at-loc.p ?t)


:goals
  ?g1 (you want1.v (that ((set-of you me) collaborate1.v)))
  ?g2 (me want1.v (that ((set-of you me) collaborate1.v)))


:episodes 

  ; David starts conversation.
  ; NOTE: this should probably include an initial high-level description of the goal structure chosen by David.
  ?e1 (me say-to.v you 
        '(OK\, I\'m ready to help with building a structure using the blocks in front of us\.
          Feel free to ask questions as we do this\.))

  ; Repeat until e2 is finished; this obtains when either the goal structure is built
  ; or the user requests to terminate the conversation prematurely.
  ?e2 (:repeat-until (:context (?e2 finished2.a))

    ; David attempts to find next step (an action type) to realize goal structure.
    ; NOTE: ?ka1 becomes bound to a reified action corresponding to the planner output;
    ; if this is done successfully, ((pair me ?e3) successful.a) is stored in context.
    ?e3 (me try1.v (to (find4.v (some ?ka1 (?ka1 step1-toward.n ?goal-rep)))))

    ; Either (4a) next step found successfully, or (4b) failure to do so.
    ?e4 (:if

        ; (4a)
        ((:context ((pair me ?e3) successful.a))

          ; Either (5a) goal structure has been reached, or (5b) goal structure not yet reached.
          ?e5 (:if

            ; (5a)
            ((:equal ?ka1 (ka (do2.v nothing.pro)))

              ; Next step is to do nothing; goal structure realized.
              ?e6 (me say-to.v you '(The goal structure is complete\.))

              ; Terminate conversation.
              ?e7 (:store-in-context '(?e2 finished2.a)))

            ; (5b)
            (:default

              ; David proposes the next step to the user.
              ?e8 (me propose1-to.v you ?ka1)

              ; Allow for user follow-up questions until the user accepts/rejects the proposal by saying something
              ; like "yes", "okay", or "no", or (ideally - see note below) just making a move.
              ?e9 (:repeat-until (:context (you accept.v ?e8))
              
                ; User says something to David. Either acknowledgement, or a query of some sort.
                ; NOTE: it's entirely possible that the user does not say anything at all, and gives
                ; "tacit" acceptance or rejection of the proposal by making the proposed move,
                ; or making a different move than the proposed one, respectively.
                ; This isn't supported in the current form of this schema, and I'm a bit at a loss as to how
                ; to "encode" these sorts of tacit statements in the framework of this schema while simultaneously
                ; allowing for iterated follow-up questions by the user.
                ?e10 (you say-to.v me ?ulf)

                ; Either (11a) user accepts proposal, (11b) user rejects proposal, (11c) makes a termination
                ; request, or (11d) asks some query.
                ?e11 (:if

                  ; (11a)
                  ((:equal ?ulf '(YES.YN))
                  
                    ; If accepted, store that the proposal has been accepted in context.
                    ?e12 (:store-in-context '(you accept.v ?e8)))

                  ; (11b)
                  ((:equal ?ulf '(NO.YN))

                    ; If rejected issue correction.
                    ; NOTE: according to this schema David is rather inflexible, simply repeating the same proposal over
                    ; and over again to the user until they do the right move. However, ideally it seems that David should
                    ; try to find a new plan consistent with the user's own proposed move, or even try to find a new goal
                    ; structure consistent with the user's proposed move. Both of these would add another level of complexity
                    ; on the current schema.
                    ?e13 (me issue-correction-to.v you ?ka1))

                  ; (11c)
                  ((:equal ?ulf '(GOODBYE.GR))
                  
                    ; If user requests to terminate conversation prematurely, store that the outer loop is finished.
                    ?e14 (:store-in-context '(?e2 finished2.a)))

                  ; (11d)
                  (:default

                    ; If the user said something other than a clear acceptance or rejection of the proposed action, David
                    ; should reply to it - instantiating the appropriate subschema such as reactions-to-historical-question
                    ; or reactions-to-spatial-question.
                    ?e15 (me reply-to.v ?e10)))))))

        ; (4b)
        (:default

          ; Failure to find next step; goal structure not possible.
          ?e16 (me say-to.v you
                  '(I cannot build the structure with the available blocks\.))

          ; For now, the dialogue just terminates in the case of a failure at any stage in the session.
          ; However, we might want to explore recovery options, like finding a new goal structure compatible
          ; with the blocks at time of failure.
          ?e17 (:store-in-context '(?e2 finished2.a)))))

  ; David says goodbye after conversation is over.
  ?e18 (me say-to.v you '(Goodbye for now!))


:event-relations
  ; Not included yet


:necessities
  ; Not included yet


:certainties
  ; Not included yet


)) ; END defparameter *eta-schema*



;````````````````````````````````````````````````````````
; Create empty hash tables for semantics,
; gist-clauses, and topic-keys
;
(setf (get '*eta-schema* 'semantics) (make-hash-table))
(setf (get '*eta-schema* 'gist-clauses) (make-hash-table))
(setf (get '*eta-schema* 'topic-keys) (make-hash-table))



;````````````````````````````````````````````````````````
; EL Semantics - Not yet used
;
(mapcar #'(lambda (x)
      (store-output-semantics (first x) (second x) '*eta-schema*))
  '()
) ; END mapcar #'store-output-semantics



;````````````````````````````````````````````````````````
; Gist clauses
;
(mapcar #'(lambda (x) 
      (store-output-gist-clauses (first x) (second x) '*eta-schema*))
  '()
) ; END mapcar #'store-output-gist-clauses



;````````````````````````````````````````````````````````
; Topic keys
;
(mapcar #'(lambda (x) 
      (store-topic-keys (first x) (second x) '*eta-schema*))
  '()
) ; END mapcar #'store-topic-keys
