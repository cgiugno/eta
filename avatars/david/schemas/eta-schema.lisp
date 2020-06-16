;; *ETA-SCHEMA*: development version 6
;;
;; Dialogue for blocks world conversation 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defparameter *eta-schema*

'(Event-schema (((set-of ~me ~you) have-eta-dialog.v) ** ?e)
;```````````````````````````````````````````````````````````
; Blocks world conversation. An expected blocks world dialogue consists of the agent repeatedly asking
; the user if they have a spatial question to ask, followed by a question from the user, followed by
; an appropriate response by the agent. This repeats until the user says something interpreted as a goodbye,
; and is possibly preempted by some "smalltalk" questions (currently disabled).
;

:types
  !t1 (~you person.n)
  !t2 (~me robot.n)
  !t3 ((the.d table.n) table.n)
  ; Eventually this should be changed to just ?bb (plur block1.n)
  !t4  ((the.d (|Target| block.n)) block.n)
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


:episodes 

; Alternate (longer) intro
;; ?e1 (~me say-to.v you 
;;       '(Hi I am David\. I\'m here to help answer some spatial questions for you\.
;;         Please try to speak as clearly as possible\. If you wish to quit at any time\, just say bye or goodbye\.))

; Short intro
?e1 (~me say-to.v ~you 
      '(Hi\, my name is David\. I\'m ready to answer your spatial questions\.))


; The following actions are excluded for now.
;; ?e2 (~me say-to.v ~you 
;;       '(Before we begin\, would you mind telling me what your name is?))

;; ?e3 (~you reply-to.v ?e2)

;; ?e4 (~me react-to.v ?e3)



; Repeat prompting the user for a spatial question until finished.
?e5 (:repeat-until (:context (?e5 finished2.a))

    ; Prompt the user for a spatial question.
    ?e7 (~me say-to.v ~you
          '(Do you have a spatial question for me?))

    ; User replies with either spatial question, special request, or smalltalk.
    ?e8 (~you reply-to.v ?e7)

    ; If user makes 'goodbye' special request, store the fact that ?e5 is finished.
    ?e9 (:if
        ((:equal (ulf-of.f ?e8) (GOODBYE.GR))
        ;; ?e11 (~me commit-to-STM.v (that (?e5 finished2.a)))
        ?e11 (:store-in-context '(?e5 finished2.a))
        ?e12 (~me react-to.v ?e8))

        ; If user makes 'pause' special request, repeat listening to the user for a request to resume.
        ((:equal (ulf-of.f ?e8) (PAUSE.GR))
        ?e12 (~me react-to.v ?e8)
        ?e13 (:repeat-until (:context (?e13 finished2.a))

            ; Empty say-to act, carries (implied) meaning "do you want to resume?". Not ideal, but currently
            ; necessary to give the user something to reply to.
            ?e14 (~me say-to.v ~you '())

            ; User replies with either special request or smalltalk.
            ?e15 (~you reply-to.v ?e14)

            ; If user makes 'resume' special request, store the fact that ?e12 is finished.
            ?e16 (:if
                ((:equal (ulf-of.f ?e15) (RESUME.GR))
                ?e17 (:store-in-context '(?e13 finished2.a))
                ?e18 (~me react-to.v ?e15))

                ; If user makes 'goodbye' special request, store the fact that both ?e5 and ?e12 are finished.
                ((:equal (ulf-of.f ?e15) (GOODBYE.GR))
                ?e17 (:store-in-context '(?e13 finished2.a) '(?e5 finished2.a))
                ?e18 (~me react-to.v ?e15))))))

  ; React to the user's input (but stay silent if they made a special request)
  ?e19 (:if
       ((:not (:or (:equal (ulf-of.f ?e8) (PAUSE.GR)) (:equal (ulf-of.f ?e8) (GOODBYE.GR))))
        ?e20 (~me react-to.v ?e8))))

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
  '(
    ;; (?e2  ((what is your name ?)))
    (?e7  ((do you have a spatial question ?)))
    (?e14 ((do you want to resume ?)))
  )
) ; END mapcar #'store-output-gist-clauses



;````````````````````````````````````````````````````````
; Topic keys
;
(mapcar #'(lambda (x) 
      (store-topic-keys (first x) (second x) '*eta-schema*))
  '(
    ;; (?e2  (name))
    (?e7  (spatial-question1))
  )
) ; END mapcar #'store-topic-keys
