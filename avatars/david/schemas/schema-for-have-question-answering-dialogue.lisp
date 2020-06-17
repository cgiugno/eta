;; *have-question-answering-dialogue*: development version 6
;;
;; Dialogue for blocks world question-answering dialogue
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defparameter *have-question-answering-dialogue*

'(Event-schema (((set-of ~me ~you) have-question-answering-dialogue.v) ** ?e)
;````````````````````````````````````````````````````````````````````````````
; Blocks world conversation. An expected blocks world dialogue consists
; of the agent repeatedly asking the user if they have a spatial question
; to ask, followed by a question from the user, followed by an appropriate
; response by the agent. This repeats until the user says something interpreted
; as a goodbye, and is possibly preempted by some "smalltalk" questions
; (currently disabled).
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

  ; David introduces himself.
  ?e1 (~me say-to.v ~you 
        '(Hi\, my name is David\. I\'m ready to answer your spatial questions\.))

  ; Repeat prompting the user for a spatial question until finished.
  ?e2 (:repeat-until (?e2 finished2.a)

    ; Prompt the user for a spatial question.
    ?e3 (~me say-to.v ~you
          '(Do you have a spatial question for me?))

    ; User replies with either spatial question, special request, or smalltalk.
    ?e4 (~you reply-to.v ?e3)

    ; Either (5a) user requests goodbye, (5b) user requests to pause, or
    ; (5c) user makes some other reply.
    ?e5 (:cond

      ; (5a)
      (((ulf-of.f ?e4) = (GOODBYE.GR))

        ; Store the fact that ?e2 is finished and react.
        ?e6 (~me commit-to-STM.v (that (?e2 finished2.a)))
        ?e7 (~me react-to.v ?e4))

      ; (5b)
      (((ulf-of.f ?e4) = (PAUSE.GR))

        ; React and instantiate pause-conversation schema.
        ?e8 (~me react-to.v ?e4)
        ?e9 ((set-of ~me ~you) pause-conversation.v))

      ; (5c)
      (:default

        ; React to user's reply (i.e., respond to smalltalk or
        ; give an answer to the user's spatial query).
        ?e10 (~me react-to.v ?e4))))

)) ; END defparameter *have-question-answering-dialogue*



;````````````````````````````````````````````````````````
; Store schema variable name under header in *schemas*
;
(store-schema-name 'have-question-answering-dialogue.v
                  '*have-question-answering-dialogue*)



;````````````````````````````````````````````````````````
; Create empty hash tables for semantics,
; gist-clauses, and topic-keys
;
(setf (get '*have-question-answering-dialogue* 'semantics) (make-hash-table))
(setf (get '*have-question-answering-dialogue* 'gist-clauses) (make-hash-table))
(setf (get '*have-question-answering-dialogue* 'topic-keys) (make-hash-table))



;````````````````````````````````````````````````````````
; EL Semantics - Not yet used
;
(mapcar #'(lambda (x)
      (store-output-semantics (first x) (second x) '*have-question-answering-dialogue*))
  '()
) ; END mapcar #'store-output-semantics



;````````````````````````````````````````````````````````
; Gist clauses
;
(mapcar #'(lambda (x) 
      (store-output-gist-clauses (first x) (second x) '*have-question-answering-dialogue*))
  '(
    (?e3  ((do you have a spatial question ?)))
  )
) ; END mapcar #'store-output-gist-clauses



;````````````````````````````````````````````````````````
; Topic keys
;
(mapcar #'(lambda (x) 
      (store-topic-keys (first x) (second x) '*have-question-answering-dialogue*))
  '(
    (?e3  (spatial-question1))
  )
) ; END mapcar #'store-topic-keys
