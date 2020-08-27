;; July 10/19 
;; ===========================================================
;;
;; For inputs, we use the question it answers to create a list
;; of simple, explicit English clauses, especially the first of 
;; which is intended to capture the "gist" of what was said,
;; i.e., the content of an utterance most likely to be needed
;; to understand the next turn in the dialogue. The intent is that
;; logical interpretations will later play that role, and this
;; has been initiated by supplying a hash table of (some) Eta 
;; output interpretations,
;;     *output-semantics*
;; (which uses keys such as (*eta-schema* ?e3) along with the
;; hash table of gist clauses,
;;     *output-gist-clauses*
;; (indexed in the same way). These tables can be used to set up
;; the 'interpretation' and 'output-gist-clauses' properties of
;; action proposition names, generated in forming plans from 
;; schemas.
;;
;; One important goal in setting up these tables is to be able
;; later to match certain user inputs to Eta question gists/
;; interpretations, to see if the inputs already answer the
;; questions, making them redundant. 
;;
;; TODO: Regarding coreference and memory, it seems like there are
;; a couple separate things:
;; 1. Eta needs a way to parameterize say-to.v actions (and the corresponding
;; gist clauses) based on previous user answers. For example, if Eta asks "what
;; was your favorite class?" and the user replies "Macroeconomics", instead of the
;; next question being "did you find your favorite class hard", it should be
;; "did you find Macroeconomics hard?"
;; 2. Eta needs a way to "trigger" bringing up past information in response to
;; a user question, perhaps based on some similarity metric
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; -*- Common-Lisp -*-

; [This is partly derivative from "doolittle", an improvement of
; Weizenbaum's ELIZA that carries information forward from the
; previous question/answer pair, makes greater use of features,
; uses more flexible, hierarchical pattern matching, and initially
; classifies inputs by their general form (instead of by keyword).]
	
; To run the program, do the following (while in the present
; eta directory):
; lisp
; (load "start")


(setf *print-circle* t) ; needed to prevent recursive loop when printing plan-step

(defstruct ds
;```````````````````````````````
; contains the following fields:
; curr-plan        : points to the currently active dialogue plan
; dialogue-history : should be three lists: surface form, gist clauses, and ULF interpretations
; reference-list   : contains a list of discourse entities to be used in ULF coref
; equality-sets    : hash table containing a list of aliases, keyed by canonical name
; gist-kb-user     : hash table of all gist clauses + associated topic keys from user
; gist-kb-eta      : hash table of all gist clauses + associated topic keys from Eta
; context          : hash table of facts that are true at Now*, e.g., <wff3>
; memory           : hash table of atemporal/"long-term" facts, e.g., (<wff3> ** E3) and (Now* during E3)
; kb               : hash table of Eta's knowledge base, containing general facts
;
; TODO: currently reference-list and equality-sets are separate things, though they serve a similar
; purpose, namely keeping track of which entities co-refer (e.g. skolem variable and noun phrase).
; These should eventually become unified once we have a systemic way of doing coreference over all
; propositions, versus the current module built over the ULF interpretation.
;
  curr-plan
  dialogue-history
  reference-list
  equality-sets
  gist-kb-user
  gist-kb-eta
  context
  memory
  kb
) ; END defstruct ds





(defun init ()
;`````````````````````````````
; Initialize global parameters
;
; TODO:
; Other global parameters used here, but whose values are set elsewhere,
; are:  ***THIS NEEDS UPDATING
;      *output-semantics*
;      *output-gist-clauses*
;      *eta-schema* (top-level schema) & possibly many subschemas
;      *reactions-to-input* (top-level choice tree for selecting a
;         schema or subtree to react to a user turn (possibly multiple
;         extracted "gist clauses")
;      *reaction-to-assertion* for individual user assertions
;      *reaction-to-question* for individual user questions
;      *interpretation-of-input* (top-level interpretation tree),
;         and many other interpretation trees (built from packets).
;      *gist-clause-trees* (top-level gist clause extraction
;         tree) and many subsidiary gist-clause extraction trees
;         (formed from corresponding packets).
;
  ; Use response inhibition via latency numbers when *use-latency* = T
  (defvar *use-latency* t)

  ; Global variable for dialogue record structure
  (defvar *ds* (make-ds))

  ; Dialogue record keeps track of three kinds of history - surface words,
  ; gist clauses, and semantic interpretations
  (let ((dialogue-history (make-hash-table :test #'equal)))
    (setf (gethash 'words dialogue-history) nil)
    (setf (gethash 'gist-clauses dialogue-history) nil)
    (setf (gethash 'semantics dialogue-history) nil)
    (setf (ds-dialogue-history *ds*) dialogue-history))

  ; Initialize hash table for aliases/equality sets
  (setf (ds-equality-sets *ds*) (make-hash-table :test #'equal))

  ; Initialize hash tables for User and Eta topic keys/gist clauses
  (setf (ds-gist-kb-user *ds*) (make-hash-table :test #'equal))
  (setf (ds-gist-kb-eta *ds*) (make-hash-table :test #'equal))

  ; Initialize fact hash tables
  (setf (ds-context *ds*) (make-hash-table :test #'equal))
  (setf (ds-memory *ds*) (make-hash-table :test #'equal))
  (setf (ds-kb *ds*) (make-hash-table :test #'equal))

  ; Load object schemas
  (load-obj-schemas)

  ; Time
  ; Stores the constant denoting the current time period. NOW0 is taken uniquely to refer
  ; to the beginning, with all moves/etc. occurring at subsequent times.
  (defparameter *time* 'NOW0)
  (store-time)
  (update-time)

  ; Time of previous episode
  ; Stores the constant denoting the time of the previous episode
  (defparameter *time-prev* *time*)

  ; Coreference mode
  ; 0 : simply reconstruct the original ulf
  ; 1 : mode 2 but excluding i.pro and you.pro from resolved references
  ; 2 : substitute most specific references only for anaphors and indexical np's (e.g. that block)
  ; 3 : substitute most specific references for all references
  (defparameter *coreference-mode* 1)

  ; Recency cutoff used when attempting coreference (i.e. the coreference
  ; module will only look this far back, in terms of turns, in the discourse
  ; history to find possible referents).
  (defparameter *recency-cutoff* 2)

  ; Certainty cutoff used to generate responses given a list of relations+certainties from the blocks world
  (defparameter *certainty-threshold* 0.7)

  ; Maximum delays used in processing speech acts
  (defparameter *delay-acknowledge.v* 10)
  (defparameter *delay-respond-to.v* 15)
  (defparameter *delay-say-to.v* 15)

  ; Number of Eta outputs generated so far (maintained
  ; for latency enforcement, i.e., not repeating a previously
  ; used response too soon).
  (defparameter *count* 0)

  ; Used for keeping track of output number in output.txt.
  (defparameter *output-count* 0)

  ; This is used to check whether some error has caused Eta to enter
  ; an infinite loop (e.g. if the plan isn't correctly updated).
  (defparameter *error-check* 0)

  ; If *read-log* is the name of some file (in logs/ directory), read and
  ; emulate that file, allowing for user corrections and saving them in a file
  ; of the same name in logs_out/ directory.
  (defparameter *read-log* nil)

  ; Log contents and pointer corresponding to current position in log.
  (defparameter *log-contents* nil)
  (defparameter *log-answer* nil)
  (defparameter *log-ptr* 0)

  ; If *live* = T, operates in "live mode" (intended for avatar
  ; system) with file IO. If *live* = nil, operates in terminal mode.
  (defparameter *live* nil)

  ; If *perceptive* = T, is capable of perceiving the world during the
  ; perceive-world.v episode in the scema (in terminal mode, the user enters
  ; a list of facts, otherwise they're provided in perceptions.lisp)
  (defparameter *perceptive* nil)

  ; If *responsive* = T, is capable of constructing natural language responses
  ; from any ULF, including generating responses from spatial relation answers in
  ; the BW system. If *responsive* = nil, the system can only form responses/reactions
  ; at the level of gist clauses, and will refrain from fully answering spatial questions.
  (defparameter *responsive* nil)

  ; If terminal mode and perceptive, keep list of block coordinates mimicking actual BW system.
  (defparameter *block-coordinates* '(
    ((the.d (|Target| block.n))      at-loc.p ($ loc :x -3.289 :y -2.454 :z 0.488))
    ((the.d (|Starbucks| block.n))   at-loc.p ($ loc :x -2.262 :y -2.438 :z 0.493))
    ((the.d (|Twitter| block.n))     at-loc.p ($ loc :x -1.254 :y -2.43  :z 0.493))
    ((the.d (|Texaco| block.n))      at-loc.p ($ loc :x -0.27  :y -2.415 :z 0.493))
    ((the.d (|McDonald's| block.n))  at-loc.p ($ loc :x  0.713 :y -2.412 :z 0.493))
    ((the.d (|Mercedes| block.n))    at-loc.p ($ loc :x  1.696 :y -2.396 :z 0.493))
    ((the.d (|Toyota| block.n))      at-loc.p ($ loc :x  2.728 :y -2.346 :z 0.493))
    ((the.d (|Burger King| block.n)) at-loc.p ($ loc :x  3.753 :y -2.323 :z 0.493))
  ))

  ; Global variables used for IO
  (defparameter *next-answer* nil)
  (defparameter *next-input* nil)
  (defparameter *next-perceptions* nil)
  (defparameter *next-ulf* nil)
  (defparameter *goal-rep* nil)
  (defparameter *obj-schemas* nil)
  (defparameter *chosen-obj-schema* nil)

) ; END init





(defun eta (read-log live perceptive responsive)
;`````````````````````````````````````````````````
; live = t: avatar mode; live = nil: terminal mode
; perceptive = t: system awaits information during perceive-world.v action
;                      (from command line if in terminal mode)
;
; Main program: Originally handled initial and final formalities,
; (now largely commented out) and controls the loop for producing,
; managing, and executing the dialog plan (mostly, reading & feature-
; annotating inputs & producing outputs, but with some subplan
; formation, gist clause formation, etc.).
;
  (init)
  (setq *read-log* read-log)
  (setq *live* live)
  (setq *perceptive* perceptive)
  (setq *responsive* responsive)
  (setq *count* 0) ; Number of outputs so far

  (when *read-log*
    (setq *log-contents* (read-log-contents *read-log*))
    (setq *log-answer* nil)
    (setq *log-ptr* -1))

  ; If '*eta-schema* is not bound, return an error
  (when (not (boundp '*eta-schema*))
    (format t "***  *eta-schema* is not bound. If using a multi-session avatar, make sure *session-number* is set.~%")
    (return-from eta nil))

  ; Initiate the dialogue plan. The schema named
  ; *eta-schema* is used to create the top-level plan
  (setf (ds-curr-plan *ds*) (init-plan-from-schema '*eta-schema* nil))

  ;; (print-current-plan-status (ds-curr-plan *ds*)) ; DEBUGGING

  ; Execution of the plan continues so long as
  ; the dialogue plan has another step
  (loop while (has-next-step? (ds-curr-plan *ds*)) do

    ; Process next action of dialogue plan
    (process-next-action (ds-curr-plan *ds*))

    ;; (print-current-plan-status (ds-curr-plan *ds*)) ; DEBUGGING
    ;; (format t " here is after the print-current-plan-status -----------~%")

    ;; (error-check :caller 'eta)

    ; Update plan state after processing the previous step
    (update-plan-state (ds-curr-plan *ds*))

    ;; (format t " here is after the plan state has been updated -----------~%")
    ;; (print-current-plan-status (ds-curr-plan *ds*)) ; DEBUGGING
  )
  
) ; END eta





(defun implement-next-plan-episode (subplan)
;``````````````````````````````````````````````
; TBC
;
; This program is called if an episode is a general event or control flow
; formula, rather than an action formula starting with "Me" or "You".
;
  (let* ((curr-step (plan-curr-step subplan)) bindings expr new-subplan var-bindings
         (ep-name (plan-step-ep-name curr-step)) (wff (plan-step-wff curr-step))
         args-list)
  
    ;; (format t "~%WFF = ~a,~% in the ETA action ~a being processed~%" wff ep-name) ; DEBUGGING

    ; Big conditional statement to determine the type of the current
    ; action, and to form the subsequent action accordingly.
    (cond

      ;`````````````````````````````
      ; Plan: Conditional
      ;`````````````````````````````
      ; Simple "if cond, do this, else do this" conditional.
      ; binding yields ((_+ (cond1 name1 wff1 name2 wff2 ... :else name3 wff3 ...)))
      ((setq bindings (bindings-from-ttt-match '(:if _+) wff))
        (setq expr (get-multiple-bindings bindings))
        ; Generate and subplan (possibly nil)
        (setq new-subplan (plan-if-else expr)))

      ;``````````````````````````````````````````
      ; Plan: Sequence of conditionals
      ;``````````````````````````````````````````
      ; An arbitrary number of conditionals which are tried in sequence.
      ; bindings yields ((_+ ((:if cond1 name1.1 wff1.1 name1.2 wff1.2 ...)
      ;                       (:if cond2 name2.1 wff2.1 name2.2 wff2.2 ...) ...
      ;                       (:else name3.1 wff3.1 name3.2 wff3.2 ...))))
      ((setq bindings (bindings-from-ttt-match '(:try-in-sequence _+) wff))
        (setq expr (get-multiple-bindings bindings))
        ; Generate a subplan for the 1st action-wff with a true condition:
        (setq new-subplan (plan-try-in-sequence expr)))

      ;`````````````````````
      ; Plan: Looping
      ;`````````````````````
      ; repeat-until, potentially other forms of loops in the future.
      ; bindings yields ((_+ (ep-var cond name1 wff1 ...)))
      ; ep-var supplies a (quoted) episode variable, cond supplies the condition of the loop,
      ; and the rest of the list is a number of name, wff pairs.
      ((setq bindings (bindings-from-ttt-match '(:repeat-until _+) wff))
        (setq expr (get-multiple-bindings bindings))
        ; Generate a subplan for the 1st action-wff with a true condition:
        (setq new-subplan (plan-repeat-until subplan ep-name expr)))

      ;````````````````````````````````
      ; Plan: Subschema Instantiation
      ;````````````````````````````````
      ; instantiation of any subschema involving both participants.
      ((setq bindings (bindings-from-ttt-match '((! (set-of ^me ^you) (set-of ^you ^me))
                                                  schema-header? (? _*)) wff))
        (setq bindings (cdr bindings))
        (setq args-list (get-multiple-bindings bindings))
        ; If episode is an obviated action, skip, otherwise generate subplan from schema name
        (cond
          ((obviated-action ep-name)
            (setq new-subplan nil))
          (t (setq new-subplan (init-plan-from-schema (schema-name! (second wff)) args-list)))))
      
      ; Unrecognizable step
      (t (format t "~%*** UNRECOGNIZABLE STEP ~a " wff) (error))
    )

    ; Return new subplan (possibly nil) and any variable bindings
    (list var-bindings new-subplan)

)) ; END implement-next-plan-episode





(defun implement-next-eta-action (subplan)
;``````````````````````````````````````````````````
; TBC (rewrite)
;
; We assume that every {sub}plan name has a 'rest-of-plan' property
; pointing to the remainder of the plan that has not been fully executed
; (i.e., the first step of this remainder has been at most partially
; executed). Further, every action name for a nonprimitive action has,
; or needs to be supplied with, a 'subplan' property pointing to the
; name of a subplan, which will again have a 'rest-of-plan' property.
; Also, the subplan name will have a 'subplan-of' property that points
; back to the name of the action it expands.
;
; We assume that this program is called only if the first action of
; the plan  is already known to be of type (^me ...), i.e., an action by Eta.
;
; TODO: IT SEEMS THAT THIS PROGRAM COULD ITSELF BE
;   REFORMULATED AS A KIND OF CHOICE TREE THAT SELECTS A SUBPLAN
;   TO EXPAND A NONPRIMITIVE ACTION THAT IT FINDS AT THE 'REST-
;   OF-PLAN' POINTER, OR, FOR PRIMITIVE ACTIONS (SAYING WORDS),
;   EXECUTES THEM. WE MIGHT ULTIMATELY DEVELOP PLANS NON-SEQUENT-
;   IALLY, SEPARATING SUCH DEVELOPMENT FROM EXECUTION OF (CURRENTLY
;   DUE) PRIMITIVE ACTIONS. SO THE ROLE OF THE PLANNING EXECUTIVE
;   WOULD BE MORE IN THE NATURE OF "PRIORITIZING" -- DECIDING WHETHER
;   TO EXECUTE THE NEXT STEP (IF PRIMITIVE), OR WHAT PLAN STEPS TO  
;   ELABORATE, MODIFY, OR SHIFT AROUND NEXT, WHILE USING CHOICE 
;   TREES FOR FINDING SUITABLE METHODS FOR ELABORATION (AND DOING 
;   FREQUENT OVERALL CONSISTENCY, PROBABILITY, AND UTILITY 
;   CALCULATIONS).
;
; If the currently due action pointed to by the 'rest-of-plan'
; property of '{sub}plan-name' is primitive (e.g., saying something),
; execute it and advance the 'rest-of-plan pointer' of '{sub}plan-name'.
; Otherwise, if the 'subplan' property of the currently due action
; is nil, generate a subplan name, point to it via the 'subplan'
; property of the currently due action, find a choice tree or subschema
; for realizing the currently due action, and initialize the subplan.
;
; No part of the new subplan is immediately executed or further
; elaborated, so that the main Eta plan manager can in principle
; check and amend the overall rest of the plan if necessary (e.g.,
; add or modify temporal constraints to avoid inconsistencies; more 
; radical changes may be warranted for optimizing overall utility).
; Any subschemas used in the elaboration process typically supply 
; multiple (^me say-to.v ^you '(...)) actions), and choice packets used
; for step elaboration typically elaborate (^me react-to.v ...) actions
; into single or multiple (^me say-to.v ^you '(...)) subactions.
;
  (let* ((curr-step (plan-curr-step subplan)) bindings expr new-subplan var-bindings
         (ep-name (plan-step-ep-name curr-step)) (wff (plan-step-wff curr-step))
         superstep ep-name1 user-ep-name user-ulf n user-gist-clauses user-gist-passage
         proposal-gist main-clause info topic suggestion query ans perceptions
         perceived-actions sk-var sk-name concept-name goal-schema)

    ;; (format t "~%WFF = ~a,~% in the ETA action ~a being processed~%" wff ep-name) ; DEBUGGING

    ; Big conditional statement to determine the type of the current
    ; action, and to form the subsequent action accordingly.
    (cond

      ;`````````````````````
      ; Eta: Saying
      ;`````````````````````
      ; e.g. yields ((_+ '(I am a senior comp sci major\, how about you?)))
      ; or nil, for non-match
      ((setq bindings (bindings-from-ttt-match '(^me say-to.v ^you _+) wff))
        (setq expr (eval-functions (get-single-binding bindings)))
        (cond
          ; If the current "say" action is a question, then use 'topic-keys'
          ; and 'gist-clauses' of current ep-name and gist-kb-user to see
          ; if question has already been answered. If so, omit action.
          ; TODO: investigate why this required advancing the plan twice...
          ((not (null (obviated-question expr ep-name)))
            (advance-plan subplan)
            (setq new-subplan nil))
          ; Primitive say-to.v act: drop the quote, say it, increment the
          ; *count* variable, advance the 'rest-of-plan' pointer, and
          ; store the turn in the dialogue history
          ((eq (car expr) 'quote)
            (setq expr (flatten (second expr)))
            (setq *count* (1+ *count*))
            (if *live* (say-words expr) (print-words expr))
            (store-turn '^me expr (get ep-name 'gist-clauses) (list (get ep-name 'semantics))
              (ds-dialogue-history *ds*)))
          ; Nonprimitive say-to.v act (e.g. (^me say-to.v ^you (that (?e be.v finished.a)))):
          ; Should probably be illegal action specification since we can use 'tell.v' for
          ; inform acts. For the moment however, handle equivalently to tell.v.
          (t (setq new-subplan (plan-tell expr)))))

      ; For now, saying something is the only primitive action, so everything
      ; beyond this point is non-primitive actions, to be expanded using choice packets.

      ;`````````````````````
      ; Eta: Reacting
      ;`````````````````````
      ; Yields e.g. ((_! EP34.)), or nil if unsuccessful.
      ((setq bindings (bindings-from-ttt-match '(^me react-to.v _!) wff))
        (setq user-ep-name (get-single-binding bindings))
        ; Get user gist clauses and ulf from bound user action
        (setq user-gist-clauses (get user-ep-name 'gist-clauses))
        (setq user-ulf (resolve-references (get user-ep-name 'semantics)))
        (format t "~% user gist clause is ~a ~%" user-gist-clauses) ; DEBUGGING
        (format t "~% user ulf is ~a ~%" user-ulf) ; DEBUGGING
        (setq new-subplan (plan-reaction-to user-gist-clauses user-ulf)))

      ; NOTE: Apart from saying and reacting, assume that Eta actions
      ; also allow telling, describing, suggesting, asking, saying 
      ; hello, and saying good-bye. 
      ;
      ; (Other speech acts may be added later, such as proposing,
      ; rejecting, praising, advising, reprimanding, acknowledging,
      ; apologizing, exclaiming, etc.)

      ;`````````````````````
      ; Eta: Telling
      ;`````````````````````
      ; e.g. telling one's name could be formulated as
      ; (^me tell.v ^you (ans-to (wh ?x (^me have-as.v name.n ?x))))
      ; and answer retrieval should bind ?x to a name. Or we could have
      ; explicit reified propositions such as (that (^me have-as.v name.n 'Eta))
      ; or (that (^me be.v ((attr autonomous.a) avatar.n))). The match variable
      ; _! will have as a binding the (wh ...) expression.
      ((setq bindings (bindings-from-ttt-match '(^me tell.v ^you _!) wff))
        (setq info (get-single-binding bindings))
        (setq new-subplan (plan-tell info)))

      ;`````````````````````
      ; Eta: Describing
      ;`````````````````````
      ; Describing, like telling, is an inform-act, but describing conveys a proposition
      ; at an abstract level (e.g. "who I am", describing one's capabilities or appearance, etc.).
      ; This involves access to knowledge in the appropriate categories, and this may then
      ; be further expanded via tell-acts.
      ;
      ; In general, describing is a severe challenge in NLG, but here it will be initially assumed
      ; that we have schemas for expanding any descriptive actions that a plan might call for.
      ; An even simpler way of packaging related sets of sentences for outputs is to just use a
      ; tell-act of type (^me tell.v ^you (meaning-of.f '(<sent1> <sent2> ...))), where the
      ; 'meaning-of.f' function applied to English sentences supplies their semantic interpretation,
      ; reified with the 'that' operator. Combining the two ideas, we can provide schemas for expanding
      ; a describe-act directly into a tell-act with a complex meaning-of.f argument.
      ((setq bindings (bindings-from-ttt-match '(^me describe-to.v ^you _!) wff))
        (setq topic (get-single-binding bindings))
        (setq new-subplan (plan-description topic)))

      ;`````````````````````
      ; Eta: Suggesting
      ;`````````````````````
      ; e.g. (that (^you provide-to.v ^me (K ((attr extended.a) (plur answer.n)))))
      ((setq bindings (bindings-from-ttt-match '(^me suggest-to.v ^you _!) wff))
        (setq suggestion (get-single-binding bindings))
        (setq new-subplan (plan-suggest suggestion)))

      ;`````````````````````
      ; Eta: Asking
      ;`````````````````````
      ; e.g. (ans-to (wh ?x (^you have-as.v major.n ?x)))
      ((setq bindings (bindings-from-ttt-match '(^me ask.v ^you _!) wff))
        (setq query (get-single-binding bindings))
        (setq new-subplan (plan-question query)))

      ;`````````````````````
      ; Eta: Saying hello
      ;`````````````````````
      ((equal wff '(^me say-hello-to.v ^you))
        (setq new-subplan (plan-saying-hello)))

      ;``````````````````````
      ; Eta: Saying good-bye
      ;``````````````````````
      ((equal wff '(^me say-bye-to.v ^you))
        (setq new-subplan (plan-saying-bye)))

      ;```````````````````````````
      ; Eta: Exiting conversation
      ;```````````````````````````
      ; NOTE: duplicated from the above (though different action arguments) -
      ; meant to reflect a more "absolute" say-bye.v action where Eta directly/abruptly
      ; exits the conversation, whereas say-bye-to.v might be used during the exchange of
      ; pleasantries and farewells at the end of a standard conversation.
      ((equal wff '(^me say-bye.v))
        (exit))

      ;`````````````````````````````````````
      ; Eta: Recalling answer from history
      ;`````````````````````````````````````
      ((setq bindings (bindings-from-ttt-match '(^me recall-answer.v _! _!1 _!2) wff))
        (setq object-locations (eval-functions (get-single-binding bindings)))
        ;; (format t "bindings: ~a~% object locations: ~a~%" (get-single-binding bindings) object-locations) ; DEBUGGING
        (setq bindings (cdr bindings))
        (setq user-ulf (get-single-binding bindings))
        (setq bindings (cdr bindings))
        (setq expr (get-single-binding bindings))
        ; Output ULF if in live mode, along with indicator that the ULF is not intended as a query.
        (write-ulf `(quote ,(list 'non-query (eval user-ulf))))
        ; Determine answers by recalling from history
        (if *responsive*
          (setq ans `(quote ,(recall-answer object-locations (eval user-ulf))))
          (setq ans '()))
        (format t "recalled answer: ~a~%" ans) ; DEBUGGING
        ; Bind ans to variable given in plan (e.g. ?ans-relations)
        (setq var-bindings (cons (list expr ans) var-bindings)))

      ;````````````````````````````````````````
      ; Eta: Seek answer from external source
      ;````````````````````````````````````````
      ((setq bindings (bindings-from-ttt-match '(^me seek-answer-from.v _! _!1) wff))
        (setq system (get-single-binding bindings))
        (setq bindings (cdr bindings))
        (setq user-ulf (get-single-binding bindings))
        ; Leaving this open in case we want different procedures for different systems
        (cond
          ((null *live*) (write-ulf user-ulf))
          ((eq system '|Blocks-World-System|) (write-ulf user-ulf))
          (t (write-ulf user-ulf))))

      ;``````````````````````````````````````````
      ; Eta: Recieve answer from external source
      ;``````````````````````````````````````````
      ((setq bindings (bindings-from-ttt-match '(^me receive-answer-from.v _! _!1) wff))
        (setq system (get-single-binding bindings))
        (setq bindings (cdr bindings))
        (setq expr (get-single-binding bindings))
        ; Leaving this open in case we want different procedures for different systems
        (cond
          (*read-log* (setq ans '()))
          ((null *responsive*) (setq ans '()))
          ((null *live*) (setq ans `(quote ,(get-answer-offline))))
          ((eq system '|Blocks-World-System|) (setq ans `(quote ,(get-answer))))
          (t (setq ans `(quote ,(get-answer)))))
        (if (not (answer-list? (eval ans)))
          (setq ans '()))
        (format t "received answer: ~a~% (for variable ~a)~%" ans expr) ; DEBUGGING
        ; Bind ans to variable given in plan (e.g. ?ans-relations)
        (setq var-bindings (cons (list expr ans) var-bindings)))

      ;````````````````````````````
      ; Eta: Conditionally saying
      ;````````````````````````````
      ((setq bindings (bindings-from-ttt-match '(^me conditionally-say-to.v ^you _! _!1) wff))
        (setq user-ulf (get-single-binding bindings))
        (setq bindings (cdr bindings))
        (setq expr (get-single-binding bindings))
        ; Generate response based on list of relations
        (cond
          ((null *responsive*) (setq ans '(Could not form final answer \: not in responsive mode \.)))
          (t (setq ans (generate-response (eval user-ulf) (eval expr)))))
        (format t "answer to output: ~a~%" ans) ; DEBUGGING
        ; If in read-log mode, append answer to list of new log answers
        (when *read-log*
          (setq *log-answer* (modify-response ans)))
        ; Create say-to.v subplan from answer
        (setq new-subplan
          (init-plan-from-episode-list
            (list :episodes (episode-var) (create-say-to-wff ans)))))

      ;````````````````````````````
      ; Eta: Proposing
      ;````````````````````````````
      ((setq bindings (bindings-from-ttt-match '(^me propose1-to.v ^you _!) wff))
        (setq expr (get-single-binding bindings))
        (cond
          ((null *responsive*) (setq proposal-gist '(Could not create proposal \: not in responsive mode \.)))
          (t (setq proposal-gist (generate-proposal expr))))
        (format t "proposal gist: ~a~%" proposal-gist) ; DEBUGGING
        (setf (get ep-name 'gist-clauses) (list proposal-gist))
        (setq new-subplan (plan-proposal proposal-gist)))

      ;````````````````````````````
      ; Eta: Issuing corrections
      ;````````````````````````````
      ; NOTE: currently equivalent to propose1-to.v, except proposals are processed differently
      ; so as to suppress corrections on 'undo' actions and add corrective phrasing.
      ((setq bindings (bindings-from-ttt-match '(^me issue-correction-to.v ^you _!) wff))
        (setq expr (get-single-binding bindings))
        (cond
          ((null *responsive*) (setq proposal-gist '(Could not create proposal \: not in responsive mode \.)))
          (t (setq proposal-gist (generate-proposal expr))))
        ;; (format t "proposal gist: ~a~%" proposal-gist) ; DEBUGGING
        (setf (get ep-name 'gist-clauses) (list proposal-gist))
        (setq new-subplan (plan-correction proposal-gist)))

      ;````````````````````````````
      ; Eta: Perceiving world
      ;````````````````````````````
      ((setq bindings (bindings-from-ttt-match '(^me perceive-world.v _! _!1 _!2) wff))
        (setq system (get-single-binding bindings))
        (setq bindings (cdr bindings))
        (setq user-ulf (get-single-binding bindings))
        (setq bindings (cdr bindings))
        (setq expr (get-single-binding bindings))
        ; Get perceptions
        (cond
          (*read-log* (setq perceptions (second (nth *log-ptr* *log-contents*))))
          ((null *perceptive*) (setq perceptions nil))
          ((null *live*) (setq perceptions (get-perceptions-offline)))
          ((eq system '|Blocks-World-System|) (setq perceptions (get-perceptions)))
          (t (setq perceptions (get-perceptions))))

        ; Ensure perceptions are in correct format, i.e. a list of lists, otherwise set to nil
        (if (or (not perceptions) (not (listp perceptions)) (not (every #'listp perceptions)))
          (setq perceptions nil))
        ; When in terminal mode and perceptive mode, update block coordinates after user gives move and add to perceptions
        (when (or *read-log* (and (null *live*) *perceptive*))
          (setq perceptions (update-block-coordinates (remove-if-not #'verb-phrase? perceptions))))
 
        (format t "received perceptions: ~a~% (for variable ~a)~%" perceptions expr) ; DEBUGGING

        ; Commit perceptions to memory
        (commit-perceptions-to-memory perceptions user-ulf)       
        
        ; Bind quoted perceptions for var in plan
        (setq var-bindings (cons (list expr `(quote ,perceptions)) var-bindings)))

      ;`````````````````````````
      ; Eta: Committing to STM
      ;`````````````````````````
      ; Storing a given wff expression in short-term memory (context)
      ; TODO: currently this action takes a single (reified) fact, e.g.,
      ; (^me commit-to-STM.v (that ((the.d (|Twitter| block.n)) blue.a))).
      ; In the future, it seems like it should be able to support a conjunction
      ; of facts using 'and', rather than having to have many separate actions for each.
      ((setq bindings (bindings-from-ttt-match '(^me commit-to-STM.v (that _!)) wff))
        (setq expr (get-single-binding bindings))
        ; Store each formula in context
        (store-in-context expr))

      ;````````````````````````````
      ; Eta: Trying
      ;````````````````````````````
      ; Forms a subplan for whichever argument is given to the try1.v
      ; action. If the subplan is successful (i.e., returns t), store
      ; ((pair ^me ?ep-var) successful.a) in context.
      ; TODO: currently doesn't do anything in particular other than
      ; making a subplan - the context storage is hardcoded into the find4.v
      ; action, which needs to be changed once I hear back from Len.
      ((setq bindings (bindings-from-ttt-match '(^me try1.v (to _!)) wff))
        (setq expr (get-single-binding bindings))
        (setq new-subplan
          (init-plan-from-episode-list
            (list :episodes (episode-var) (cons '^me expr)))))

      ;````````````````````````````
      ; Eta: Finding
      ;````````````````````````````
      ; Finding some action (or other entity?), given an episode like
      ; ?e1 (find4.v (some.d ?ka1 (:l (?x) (?x step1-toward.p ?goal-rep))))
      ; TODO: currently manually stores ((pair ^me ?e1) successful.a), which
      ; needs to be changed - see note on 'Eta: Trying' action.
      ; Also sends query to the BW system for step regardless of what the
      ; argument of the find4.v action is.
      ((setq bindings (bindings-from-ttt-match '(^me find4.v _!) wff))
        (setq expr (get-single-binding bindings))
        (setq sk-var (second expr))
        (observe-step-towards-goal (third (third (third expr)))); desperately needs changing
        (setq sk-name (choose-variable-restrictions sk-var (third expr)))
        (format t "found ~a for variable ~a~%" sk-name sk-var)
        ;; (setq superstep (plan-subplan-of subplan))
        ;; (setq ep-name1 (plan-step-ep-name superstep))
        (setq ep-name1 ep-name)
        (if (and sk-name ep-name1)
          (store-in-context `((pair ^me ,ep-name1) successful.a))); also desperately needs changing

        ; Bind variable to skolem constant in plan
        (setq var-bindings (cons (list sk-var sk-name) var-bindings)))

      ;````````````````````````````
      ; Eta: Choosing
      ;````````````````````````````
      ; Choosing a reference for an indefinite quantifier, given an episode
      ; like ?e1 (^me choose.v (a.d ?c (random.a
      ;            (:l (?x) (and (?x member-of.p ?cc)
      ;                          (not (^you understand.v ?x)))))))
      ; The lambda abstract is used to select candidates (given the facts stored
      ; in context), which is optionally preceded by an additional modifier
      ; (e.g., random.a, (most.mod-a simple.a), etc.) which is used for final
      ; selection from the candidate list.
      ; The canonical name is substituted for the variable in the rest of the plan
      ; (a skolem constant is also generated and aliased to the canonical name, but
      ; is currently unused).
      ((setq bindings (bindings-from-ttt-match '(^me choose.v _!) wff))
        (setq expr (get-single-binding bindings))
        (setq sk-var (second expr))
        (setq sk-name (choose-variable-restrictions sk-var (third expr)))
        (format t "chose value ~a for variable ~a~%" sk-name sk-var) ; DEBUGGING

        ; Bind variable to skolem constant in plan
        (setq var-bindings (cons (list sk-var sk-name) var-bindings)))

      ;```````````````````````````````````````
      ; Eta: Forming a spatial representation
      ;```````````````````````````````````````
      ; Form some spatial representation of a concept (i.e., of an
      ; object schema). Given an episode like:
      ; ?e2 (^me form-spatial-representation.v (a.d ?goal-rep ((most.mod-a simple.a)
      ;        (:l (?x) (and (?x goal-schema1.n) (?x instance-of.p ?c))))))
      ; First, Eta queries the BW system for the spatial representation, using the indefinite
      ; quantifier. Then, Eta reads the goal representation from the BW system, generates a name for
      ; the goal representation, and substitutes it in the schema.
      ((setq bindings (bindings-from-ttt-match '(^me form-spatial-representation.v _!) wff))
        (setq expr (get-single-binding bindings))
        (setq sk-var (second expr))
        ; Substitute record structure for concept name in expr
        (setq expr (ttt:apply-rule '(/ (_!1 instance-of.p _!2)
                                       (_!1 instance-of.p (record-structure! _!2)))
                      expr :max-n 1))
        ; Request goal representation from BW system
        (request-goal-rep expr)
        ; Get goal representation from BW system
        ; NOTE: currently no special offline (terminal mode) procedure for getting goal schema.
        (setq goal-schema (get-goal-rep))
        ; Generate skolem name, add alias and facts in context
        (setq sk-name (gensym "BW-goal-rep"))
        (add-alias (cons '$ goal-schema) sk-name)
        (store-in-context (list sk-name 'goal-schema1.n))
        (store-in-context (list sk-name 'instance-of.p concept-name))
        ; Substitute skolem name for skolem var in schema
        (format t "formed representation ~a for variable ~a~%" sk-name sk-var)
        
        ; Bind variable to skolem constant in plan
        (setq var-bindings (cons (list sk-var sk-name) var-bindings)))

      ;````````````````````````````
      ; Eta: Initiating Subschema
      ;````````````````````````````
      ((setq bindings (bindings-from-ttt-match '(^me schema-header? ^you (? _*)) wff))
        (setq args-list (get-multiple-bindings bindings))
        ; If episode is an obviated action, skip, otherwise generate subplan from schema name
        (cond
          ((obviated-action ep-name)
            (setq new-subplan nil))
          (t (setq new-subplan (init-plan-from-schema (schema-name! (second wff)) args-list)))))
      
      ; Unrecognizable step
      (t (format t "~%*** UNRECOGNIZABLE STEP ~a " wff) (error))
    )

    ; Return new subplan (possibly nil) and any variable bindings
    (list var-bindings new-subplan)

)) ; END implement-next-eta-action





(defun observe-next-user-action (subplan)
;```````````````````````````````````````````````
; TBC (rewrite)
;
; '{sub}plan-name' provides the name of a (sub)plan whose
; 'rest-of-plan' pointer points to a user action, i.e., the
; name of a user action followed by a wff of type (^you ...).
;
; We build a two-level plan structure for nonprimitive user
; replies (with a (^you say-to.v ^me '(...)) at the primitive
; level), and (in another Eta plan iteration) "interpret" 
; these replies. The value returned is a pair 
;    (<user action name> <corresponding wff>) 
; for the step that was processed. (This is not needed but 
; may help in debugging.)
;
; The idea is that we should recognize user actions as being
; hierarchically organized (just like Eta actions). 
; Currently we're just anticipating nonprimitive top-level
; actions like 
;        (^you reply-to.v <eta action>)
; that we expand to one further, primitive level of type
;        (^you say-to.v ^me '(...)) 
; actions. However, in principle, observing a user action
; is a plan-recognition process, where for example multiple 
; sentences uttered by the user may comprise a sequence of
; speech acts of different types (just like outputs by Eta);
; as well, the highest-level user plan that is recognized may
; fail to match the *expected* type of the user action (but
; we're ignoring this possibility for now).
;
; Primitive user actions arise in two ways: First, (^you say-to.v
; ^me '(...)) actions are generated here from nonprimitive
; (^you reply-to.v ...) actions as already mentioned and explained
; further below. Second, Eta actions of type (^me react-to.v ...)
; may generate schema-based subplans that contain multiple Eta
; comments of type (^me say-to.v ^you '(...)), where these are
; preceded by "hallucinated" user inputs of form (^you paraphrase.v
; '(...)); here the quoted words comprise a gist clause "attributed"
; to the user, i.e., these are treated as implicit versions of 
; (parts of) the user's previous actual input that were "para-
; phrased" by the user in the context of the Eta question they
; answer. These "hallucinated" clauses attributed to the user are
; needed to enable uniform processing of Eta's reaction to each
; individual gist clause derived from an actual input.
;
; To generate a subplan containing a primitive (^you say-to.v ^me 
; '(...)) action, given a (^you reply-to.v <eta action>) action, 
; we read the user input, form a wff for the primitive action with
; the input word list filled in, generate a plan name for the
; simple subordinate plan, and assign a value to that plan name
; consisting of a new action name for the primitive action and the
; (^you say-to.v ...) wff. We don't make interpretation of the 
; user input part of the process of generating the primitive 
; action (though we could, since we have at hand the <eta action>
; to which the user is responding, in the wff (^you reply-to.v 
; <eta action>)); instead, we derive the interpretation when
; processing the primitive action; this is for consistency with
; the general principle that interpretation (including speech act
; recognition) should proceed bottom-up (but with the previous 
; Eta utterance as context). [However, maybe hierarchical
; interpretation should be a process separate from hierarchical
; plan processing...]
; 
; So, processing of primitive (^you say-to.v ^me '(...)) actions
; should lead to their "interpretation", i.e., extraction of gist
; clauses and possibly supplementary information that could
; obviate later Eta questions. This requires finding out what
; the user is replying to, by looking "upward" and "backward" in the
; plan hierarchy. Specifically, we need to access the nonprimitive
; user action that immediately subsumes the (^you say-to.v ^me ...)
; action -- this is accessible via the 'subplan-of' property of
; {sub}plan-name -- and the wff of this noprimitive action in turn
; supplies the name of the Eta action that the user is responding
; to. The 'gist-clauses' property of that Eta action name leads 
; to the desired context information for interpreting the user input 
; utterance. (In future the 'interpretation' property is to be used.)
; 
  (let* ((curr-step (plan-curr-step subplan)) bindings expr new-subplan var-bindings
         (ep-name (plan-step-ep-name curr-step)) (wff (plan-step-wff curr-step))
         words superstep ep-name1 wff1 wff1-arg eta-ep-name eta-clauses user-gist-clauses
         main-clause user-ulfs user-action input user-try-ka-success)

    ;; (format t "~%WFF = ~a,~%      in the user action ~a being processed~%" wff ep-name) ; DEBUGGING

    ; Big conditional statement to observe different types of user actions
    (cond
      ;`````````````````````
      ; User: Saying
      ;`````````````````````
      ; A say-to.v act may be primitive, having a '?words' variable as its argument,
      ; or may have been created from a (^you reply-to.v <eta action>)) based on reading
      ; the user's input:
      ((setq bindings (bindings-from-ttt-match '(^you say-to.v ^me _!) wff))
        (setq expr (get-single-binding bindings))

        (cond
          ; if say-to.v act has a variable argument, e.g. '?words', it should read the user's
          ; words, and destructively substitute these words in for the variable in the plan.
          ((variable? expr)
            (loop while (not input) do
              (setq input (cond
                (*read-log* (if (>= *log-ptr* (length *log-contents*))
                  (read-words "bye")
                  (read-words (first (nth *log-ptr* *log-contents*)))))
                (*live* (hear-words *delay-say-to.v*))
                (t (read-words)))))
            (setq words (decompress input))
            ; Bind input words to variable in plan
            (setq var-bindings (cons (list expr `(quote ,input)) var-bindings)))

          ; if say-to.v act has a quoted word list argument, e.g., '(nice to meet you \.)',
          ; it should unquote and decompress the words.
          ((quoted-sentence? expr)
            (setq words (decompress (second expr))))

          ; anything else is unexpected
          (t
            (format t "~%*** SAY-ACTION ~a~%    BY THE USER SHOULD SPECIFY A QUOTED WORD LIST OR VARIABLE" expr)))

        ; Prepare to "interpret" 'words', using the Eta output it is a response to;
        ; first we need the superordinate action (wff should be in the form (^you reply-to.v <eta action>))
        (setq superstep (plan-subplan-of subplan))
        (when superstep
          (setq ep-name1 (plan-step-ep-name superstep))
          (setq wff1 (plan-step-wff superstep))
          (setq wff1-arg (car (last wff1))))

        ;; (format t "~%User action name1 = ~a" ep-name1)
        ;; (format t "~%User words = ~a" words)
        ;; (format t "~%User WFF1 = ~a, if correct,~%    ends in a ETA action name" wff1) ; DEBUGGING
        
        (cond
          ; If the superordinate reply-to.v action has specific gist clause(s)
          ((quoted-sentence-list? wff1-arg)
            (setq eta-clauses (eval wff1-arg)))
          ; If the superordinate reply-to.v action has an episode with gist clause(s) associated
          ((symbolp wff1-arg)
            (setq eta-ep-name wff1-arg)
            (setq eta-clauses (get eta-ep-name 'gist-clauses)))
          ; Otherwise, assume there is no superordinate action, set gist clauses to nil
          (t (setq eta-clauses nil)))

        ;; (format t "~%ETA action name is ~a" eta-ep-name)
        ;; (format t "~%ETA gist clauses that the user is responding to~% = ~a " eta-clauses)
        ;; (format t "~%using gist clause: ~a " (car (last eta-clauses))) ; DEBUGGING

        ; Compute the "interpretation" (gist clauses) of the user input,
        ; which will be done with a gist-clause packet selected using the
        ; main Eta action clause, and with the user input being the text
        ; to which the tests in the gist clause packet (tree) are applied.
        ;
        ; TODO: In the future, we might instead of or in addition use
        ; (get eta-ep-name 'interpretation).
        (setq user-gist-clauses
          (form-gist-clauses-from-input words (car (last eta-clauses))))

        ; Remove contradiction
        (setq user-gist-clauses (remove-contradiction user-gist-clauses))
        (format t "Obtained user gist clauses ~a for episode ~a~%" user-gist-clauses ep-name1) ; DEBUGGING

        ; Both the primitive user action and the immediately subordinate action
        ; recieve the gist-clause interpretation just computed.
        (setf (get ep-name 'gist-clauses) user-gist-clauses)
        (setf (get ep-name1 'gist-clauses) user-gist-clauses)

        ; Get ulfs from user gist clauses and set them as an attribute to the current
        ; user action
        (setq user-ulfs (mapcar #'form-ulf-from-clause user-gist-clauses))
        (format t "Obtained ulfs ~a for episode ~a~%" user-ulfs ep-name1) ; DEBUGGING

        (setf (get ep-name 'semantics) user-ulfs)
        (setf (get ep-name1 'semantics) user-ulfs)

        ; Get fine-grained user action type corresponding to gist clauses (e.g. (^you say-be.v)),
        ; and store ((^you say-bye.v) * ?e1), where ?e1 is the ep-name of of say-to.v (and likewise
        ; for the parent of the say-to.v episode, if it exists).
        ; NOTE: for now, we assume that each user utterance is described by only one action type,
        ; so the gist clauses are concatenated together first.
        (setq user-action (form-user-action-type (apply #'append user-gist-clauses)))
        (format t "Obtained user-action ~a for episode ~a~%" user-action ep-name1) ; DEBUGGING
        (store-in-context `((^you ,user-action) * ,ep-name))
        (when ep-name1
          (store-in-context `((^you ,user-action) * ,ep-name1)))

        ; Add turn to dialogue history
        (store-turn '^you words user-gist-clauses user-ulfs (ds-dialogue-history *ds*)))

      ;`````````````````````
      ; User: Paraphrasing
      ;`````````````````````
      ; Next we deal with gist clauses "attributed" to the user, in user
      ; actions of form '(^you paraphrase.v '<gist clause>')' in a subplan
      ; derived from a schema for handling complex user turns; i.e. we take the
      ; view that the user paraphrased these gist clauses in his/her original, 
      ; often "condensed", sentences; thus we can directly set the 'gist-clauses'
      ; properties of the user action rather than applying 'form-gist-clauses-from-input'
      ; again (as was done above for (^you say-to.v ^me '(...)) actions).
      ((setq bindings (bindings-from-ttt-match '(^you paraphrase.v _!) wff))
        (setq expr (get-single-binding bindings))
        (cond
          ((not (quoted-sentence? expr))
            (format t "~%*** PARAPHRASE-ACTION ~a~%    BY THE USER SHOULD SPECIFY A QUOTED WORD LIST" expr))
          (t
            ; Drop quote, leaving a singleton list of clauses
            (setq user-gist-clauses (cdr expr))
            (setf (get ep-name 'gist-clauses) user-gist-clauses))))

      ;`````````````````````
      ; User: Replying
      ;`````````````````````
      ; Nonprimitive (^you reply-to.v <eta action name>) action; we particularize this
      ; action as a subplan, based on reading the user's input
      ((setq bindings (bindings-from-ttt-match '(^you reply-to.v _!) wff))
        ; If in *read-log* mode, finish processing the previous turn-tuple before moving on
        (when *read-log*
          (when (>= *log-ptr* 0)
            (if (not *log-answer*) (setq *log-answer* '(PARSE FAILURE \.)))
            (verify-log *log-answer* (nth *log-ptr* *log-contents*) *read-log*)
            (setq *log-answer* nil))
          (setq *log-ptr* (1+ *log-ptr*)))

        (loop while (not input) do
          
          ;; Read user input
          (setq input (cond
            (*read-log* (if (>= *log-ptr* (length *log-contents*))
              (read-words "bye")
              (read-words (first (nth *log-ptr* *log-contents*)))))
            (*live* (hear-words))
            (t (read-words)))))

        ;; (format t "~% input is equal to ~a ~%" input) ; DEBUGGING

        (cond
          ((null input) (setq new-subplan nil))
          (t
            ; Make sure that any final punctuation, such as ?, ., or !,
            ; is separated from the final word (so as to not impair pattern matching)
            (setq input (detach-final-punctuation input))

            ;; (format t "~%echo of input: ~a" input) ; DEBUGGING

            ; Create subplan
            (setq new-subplan
              (init-plan-from-episode-list
                (list :episodes (episode-var) (create-say-to-wff input :reverse t)))))))

      ;`````````````````````
      ; User: Responding
      ;`````````````````````
      ; Currently, this is treated as a more "general" version of replying, and also
      ; is aimed at more instantaneous verbal responses, i.e., occurring within some
      ; seconds after the previous action.
      ; TODO: ultimately, it seems like a response could be verbal or non-verbal (e.g.
      ; a gesture, following an instruction, etc.). But this invites certain parallel
      ; processing issues, so currently I keep these separate. Also, it seems like the
      ; distinction between replying and responding is somewhat arbitrary currently...
      ((setq bindings (bindings-from-ttt-match '(^you respond-to.v _!) wff))
        ; Read user input (within some secs if live mode)
        (setq input (if *live* (hear-words :delay *delay-respond-to.v*) (read-words)))
        (format t "~% input is equal to ~a ~%" input) ; DEBUGGING

        (cond
          ((null input) (setq new-subplan nil))
          (t
            ; Make sure that any final punctuation, such as ?, ., or !,
            ; is separated from the final word (so as to not impair pattern matching)
            (setq input (detach-final-punctuation input))

            ;; (format t "~%echo of input: ~a" input) ; DEBUGGING

            ; Create subplan
            (setq new-subplan
              (init-plan-from-episode-list
                (list :episodes (episode-var) (create-say-to-wff input :reverse t)))))))

      ;`````````````````````
      ; User: Acknowledging
      ;`````````````````````
      ; Same as replying, but allows for "tacit approval" (some secs of silence) as well.
      ((setq bindings (bindings-from-ttt-match '(^you acknowledge.v _!) wff))
        ; Read user input (within some secs if live mode)
        (setq input (if *live* (hear-words :delay *delay-acknowledge.v*) (read-words)))
        (format t "~% input is equal to ~a ~%" input) ; DEBUGGING

        (cond
          ((null input) (setq new-subplan nil))
          (t
            ; Make sure that any final punctuation, such as ?, ., or !,
            ; is separated from the final word (so as to not impair pattern matching)
            (setq input (detach-final-punctuation input))

            ;; (format t "~%echo of input: ~a" input) ; DEBUGGING

            ; Create subplan
            (setq new-subplan
              (init-plan-from-episode-list
                (list :episodes (episode-var) (create-say-to-wff input :reverse t)))))))

      ;````````````````````````````
      ; User: Trying
      ;````````````````````````````
      ; User tries some reified action. Queries the BW system for whether or
      ; not trying the action was successful.
      ; TODO: needs to be made more general in the future.
      ((setq bindings (bindings-from-ttt-match '(^you try1.v _!) wff))
        (setq expr (get-single-binding bindings))
        (setq user-try-ka-success (if *live* (get-user-try-ka-success)
                                             (get-user-try-ka-success-offline)))
        (format t "~% user-try-ka-success is equal to ~a ~%" user-try-ka-success) ; DEBUGGING
        (when user-try-ka-success
          (store-in-context `((pair ^you ,ep-name) successful.a))
          (store-in-context `((pair ^you ,ep-name) instance-of.p ,expr)))

        ; As a subplan of this, Eta should percieve the world (assuming a BW system).
        (setq new-subplan
          (init-plan-from-episode-list
            (list :episodes (episode-var)
              '(^me perceive-world.v |Blocks-World-System| nil ?perceptions)))))

      ; Unrecognizable step
      (t (format t "~%*** UNRECOGNIZABLE STEP ~a " wff) (error))
    )

    ; Return new subplan (possibly nil) and any variable bindings
    (list var-bindings new-subplan)

)) ; END observe-next-user-action





(defun plan-if-else (expr)
;``````````````````````````````````````````
; expr = (cond name1 wff1 name2 wff2 ... :else name3 wff3 name4 wff4 ...)
; Expr is a condition followed by consecutive name & wff pairs. Optionally,
; this is followed by an :else keyword and additional name & wff pairs.
;
  (let* ((cnd (car expr)) (rst (cdr expr))
         (else-episodes (car (get-keyword-contents rst '(:else))))
         (if-episodes (if (not else-episodes) rst
          (reverse (set-difference rst (member :else rst))))))
    (cond
      ; Try conditional
      ((eval-truth-value cnd)
        (init-plan-from-episode-list (cons :episodes if-episodes)))
      ; Otherwise try else, if it exists
      (else-episodes
        (init-plan-from-episode-list (cons :episodes else-episodes)))))
) ; END plan-if-else





(defun plan-try-in-sequence (expr)
;``````````````````````````````````````````````````
; expr = ((:if cond1 name1.1 wff1.1 name1.2 wff1.2 ...)
;         (:if cond2 name2.1 wff2.1 name2.2 wff2.2 ...) ...
;         (:else name3.1 wff3.1 name3.2 wff3.2 ...))
; Expr is a list of consecutive (:if cond e1 e2 ...) lists, potentially followed
; by a final (:else e1 e2 ...) list. These conditions should be tried in sequence,
; instantiating the first one which holds true.
;
  (let* ((lst1 (car expr)) (else1 (if (equal (first lst1) :else) t))
         (cond1 (if (not else1) (second lst1)))
         (episodes1 (if (not else1) (cddr lst1) (cdr lst1))))
    (cond
      ; None of the cases have been matched, so no subplan is generated
      ((null expr) nil)
      ; If the condition is satisfied, create a subplan from the episode list
      ((or else1 (eval-truth-value cond1))
        (init-plan-from-episode-list (cons :episodes episodes1)))
      ; Otherwise, try next condition & episodes
      (t (plan-try-in-sequence (cdr expr))))
)) ; END plan-try-in-sequence





(defun plan-repeat-until (subplan ep-name expr)
;`````````````````````````````````````````````````````````
; expr = (ep-var cond name1 wff1 name2 wff2 ...)
;
; 'ep-name' is the name of the reoccuring :repeat-until episode. It will
; be used again in the recursion at the end of the plan we are forming;
; 'expr' is of form
;    (ep-var cond name1 wff1 name2 wff2 ...), 
; where cond is the stop condition of the repeated event,
; 'name1' is the episode characterized by the first action- or event-wff
; 'wff1', 'name2' is the episode characterized by the 2nd action- or
; event-wff 'wff2', etc.
;
; The subplan (if wff0 is false) will consist of all the steps of the loop (with
; duplicate action names created, which inherit any attached gist clauses/ulf/etc.), 
; and ending with another repeat-until loop, identical to the original one.
;
; TODO: I THINK I'LL ALSO NEED 'plan-seq-acts', 'plan-consec-acts', ETC.
; THESE SHOULD BE PRETTY SIMPLE, JUST LISTING THE ACTIONS & PROVIDING
; seq-ep, consec-ep, ETC. RELATIONS IN THE SUBPLAN. 
;
  (let ((cond1 (first expr)) (expr-rest (cdr expr)) truth-val new-subplan)
    ; First check termination condition
    (setq truth-val (eval-truth-value cond1))
    ; Substitute expr-rest with duplicate variables
    (setq expr-rest (subst-duplicate-variables subplan expr-rest))
    (cond
      ; Termination has been reached - return nil so the calling program can delete loop
      (truth-val nil)
      ; Otherwise, create a subplan that has the steps of the loop & a recursive copy of the loop
      (t (setq new-subplan
          (init-plan-from-episode-list
            (cons :episodes (append expr-rest (list ep-name (cons :repeat-until expr))))))
        new-subplan))
)) ; END plan-repeat-until





(defun plan-reaction-to (user-gist-clauses user-ulf)
;````````````````````````````````````````````````````````````````
; Starting at a top-level choice tree root, choose an action or
; subschema suitable for reacting to 'user-gist-clauses' (which
; is one or more sentences, without tags (and with a final detached
; "\." or "?"), that try to capture the main content (gist) of
; a user input). Return the (new) name of a plan for realizing 
; that action or subschema.
;
; If the action arrived at is a particular verbal output (instantiated
; reassembly pattern, where the latter was signalled by directive :out, 
; & is indicated by ':out' in the car of the 'choose-result-for' result), 
; form a plan with one action, viz. the action of saying that verbal 
; output.
;
; If the action arrived at is another choice tree root (signalled by
; directive :subtree), this will be automatically pursued recursively
; in the search for a choice, ultimately delivering a verbal output
; or a schema name.
;
; If the action arrived at is a :schema+args "action" (a schema name
; along with an argument list), use this schema to form a subplan.
;
; ** Should the new subplan name also receive a 'semantics'
; property? ... We don't really expect a further user response to these
; reactive comments from Eta, which would then need to be understood
; in light of the meaning of these reactive comments...More thought
; required.
;
  (let (user-gist-words choice tagged-words wff schema-name args)
    
    (if (null user-gist-clauses)
      (return-from plan-reaction-to nil))

    ; Currently we're only using a single ulf
    (if user-ulf (setq user-ulf (car user-ulf)))

    ; If the extracted ulf specifies an :out directive, we want to create a
    ; say-to.v subplan directly
    ; TODO: this is not very elegant - not sure it makes sense for the value
    ; of the user's ULF to be an out directive by Eta...
    (when (and user-ulf (eq (car user-ulf) :out))
      (return-from plan-reaction-to
        (init-plan-from-episode-list
          (list :episodes (episode-var) (create-say-to-wff (cdr user-ulf))))))

    ; Remove 'nil' gist clauses (unless the only gist clause is the 'nil' gist clause)
    (setq user-gist-clauses_p (purify-func user-gist-clauses))

    ; We use either choice tree '*reaction-to-input*' or
    ; '*reactions-to-input*' (note plural) depending on whether
    ; we have one or more gist clauses.
    (cond
      ; Single gist clause
      ((null (cdr user-gist-clauses_p))
        (setq tagged-words (mapcar #'tagword (car user-gist-clauses_p)))
        ;; (format t "~% (single clause) tagwords are ~a ~% " tagged-words) ; DEBUGGING
        (setq choice (choose-result-for tagged-words '*reaction-to-input*))
        ;; (format t "~% (single clause) choice are ~a ~% " choice) ; DEBUGGING
      )

      ; Multiple gist clauses
      (t
        ;; (format t "~% user-gist-words are ~a ~% " user-gist-clauses_p) ; DEBUGGING
        (setq user-gist-words (apply 'append user-gist-clauses_p))
        ;; (format t "~% user-gist-words are ~a ~% " user-gist-words) ; DEBUGGING
        (setq tagged-words (mapcar #'tagword user-gist-words))
        ;; (format t "~% tagwords are ~a ~% " tagged-words) ; DEBUGGING
        (setq choice (choose-result-for tagged-words '*reactions-to-input*))
        ;; (format t "~% choice is ~a ~% " choice) ; DEBUGGING
    ))

    (if (null choice) (return-from plan-reaction-to nil))

    ; 'choice' may be an instantiated reassembly pattern (prefaced by
    ; directive :out), or the name of a schema (to be initialized).
    ; In the first case we create a 1-step subplan whose action is of
    ; the type (^me say-to.v ^you '(...)), where the verbal output is
    ; adjusted by applying 'modify-response' to the reassembly patterns.
    ; In the second case, we initiate a multistep plan.
    (cond
      ; :out directive
      ((eq (car choice) :out)
        (init-plan-from-episode-list
          (list :episodes (episode-var) (create-say-to-wff (cdr choice)))))

      ; :schema directive
      ((eq (car choice) :schema)
        (setq schema-name (cdr choice))
        (init-plan-from-schema schema-name nil))

      ; :schema+args directive
      ((eq (car choice) :schema+args)
        ; We assume that the cdr of 'choice' must then be of form
        ; (<schema name> <argument list>)
        ; The idea is that the separate pieces of the word sequence
        ; supply separate gist clauses that Eta may react to in the
        ; steps of the schema. These are provided as sublists in 
        ; <argument list>.
        (setq schema-name (first (cdr choice)) args (second (cdr choice)))
        (init-plan-from-schema schema-name args))

      ; :schema+ulf directive
      ((eq (car choice) :schema+ulf)
        ; TODO: Just a temporary directive to test spatial-question schema. Needs changing.
        (setq schema-name (cdr choice) args (list `(quote ,(resolve-references user-ulf)) nil))
        (init-plan-from-schema schema-name args))
    )
)) ; END plan-reaction-to





(defun plan-proposal (proposal-gist)
;````````````````````````````````````````````````
; Given a proposal gist clause, convert it to an utterance using
; hierarchical transduction trees, starting at a top-level choice
; tree root.
; NOTE: currently only :out directives are expected, but this can
; be expanded if we find e.g. subschema instantiation is necessary
; (for example, if some particularly complex proposal that needs
; to be broken down into multiple actions).
;
  (let (tagged-words choice)

    (if (null proposal-gist)
      (return-from plan-proposal nil))

    (setq tagged-words (mapcar #'tagword proposal-gist))
    ;; (format t "~% proposal tagwords are ~a ~% " tagged-words) ; DEBUGGING
    (setq choice (choose-result-for tagged-words '*output-for-proposal-tree*))
    ;; (format t "~% proposal choice is ~a ~% " choice) ; DEBUGGING

    (when (or (null choice) (equal choice '(:out)))
      (return-from plan-proposal nil))

    (cond
      ; :out directive
      ((eq (car choice) :out)
        (init-plan-from-episode-list
          (list :episodes (episode-var) (create-say-to-wff (cdr choice))))))
)) ; END plan-proposal





(defun plan-correction (correction-gist)
;````````````````````````````````````````````````````
; Given a correction gist clause, convert it to an utterance using
; hierarchical transduction trees, starting at a top-level choice
; tree root.
; NOTE: the same as plan-proposal, but uses a different rule tree.
;
  (let (tagged-words choice)

    (if (null correction-gist)
      (return-from plan-correction nil))

    (setq tagged-words (mapcar #'tagword correction-gist))
    ;; (format t "~% correction tagwords are ~a ~% " tagged-words) ; DEBUGGING
    (setq choice (choose-result-for tagged-words '*output-for-correction-tree*))
    ;; (format t "~% correction choice is ~a ~% " choice) ; DEBUGGING

    (when (or (null choice) (equal choice '(:out)))
      (return-from plan-correction nil))

    (cond
      ; :out directive
      ((eq (car choice) :out)
        (init-plan-from-episode-list
          (list :episodes (episode-var) (create-say-to-wff (cdr choice))))))
)) ; END plan-correction





(defun plan-tell (info) ; TBC
;`````````````````````````````
; Return the name of a plan for telling the user the 'info';
; 'info' is a reified proposition that may be in a form that makes
; verbalization trivial, e.g.,
;     (meaning-of.f '(I am Eta. I am an autonomous avatar.))
; where the 'meaning-of.f' function in principle provides EL
; propositions corresponding to English sentences -- i.e., semantic
; parser output, reified using 'that'; but of course, for verbal-
; ization we don't need to first convert to EL! Or else the info 
; is directly in EL form, e.g.,
;     (that (^me have-as.v name.n 'Eta)), or
;     (that (^me be.v ((attr autonomous.a) avatar.n))),
; which requires English generation for a fully expanded tell
; act.
;
  (if (null info) (return-from plan-tell nil))
  ; TBC
) ; END plan-tell





(defun plan-description (topic) ; TBC
;`````````````````````````````````````
  (if (null info) (return-from plan-description nil))
  ; TBC
) ; END plan-description





(defun plan-suggest (suggestion) ; TBC
;````````````````````````````````````````
  (if (null suggestion) (return-from plan-suggest nil))
  ; TBC
) ; END plan-suggest





(defun plan-question (query) ; TBC
;```````````````````````````````````
  (if (null query) (return-from plan-question nil))
  ; TBC
) ; END plan-question





(defun plan-saying-hello () ; TBC
;`````````````````````````````````
  ; TBC
) ; END plan-saying-hello





(defun plan-saying-bye () ; TBC
;```````````````````````````````
  ; TBC
) ; END plan-saying-bye





(defun observe-variable-type (sk-var type)
;```````````````````````````````````````````
; Through observation of the world, find an entity which can fill in
; variable of type (variable assumed to be neither in schema header or
; filled in at later point in schema).
;
  (let (sk-name sk-const)
    (setq sk-const (skolem (implode (cdr (explode sk-var)))))
    (setq sk-name
      (car (find-all-instances-context `(:l (?x) (?x ,type)))))
    (add-alias sk-const sk-name)
    sk-name)
) ; END observe-variable-type





;; (defun form-spatial-representation ()
;; ;``````````````````````````````````````
;; ; Forms a spatial representation from the currently chosen BW-concept.n
;; ; (assuming such a choice has actually been made at this point), through
;; ; sending the BW system the chosen concept schema and receiving a goal
;; ; schema.
;; ; TODO: I'm not sold on how this is done currently, but I'm stumped on
;; ; how to do it more sensibly. The issue is that, for the lambda expression
;; ; + find-all-instances to work, the facts about the goal schema need to be
;; ; stored ahead-of-time, so the communication of the goal schema needs to be
;; ; done before the 'choice' step of the indefinite quantifier. This requires
;; ; sending the BW system the chosen concept schema, but the concept schema
;; ; name is nested inside the lambda extract, and messing with that here would
;; ; be a pretty messy approach. Instead, I check context for some individual such
;; ; that it is a BW-concept.n and Eta has chosen it.
;; ; TODO: BW-concept.n in lambda descr should be valid, reachable through subsumption
;; ; relationship between BW-concept.n and BW-concept-structure.n/BW-concept-primitive.n
;; ; in noun hierarchy.
;; ;
;;   (let (concept-name goal-schema goal-name)
;;     (setq concept-name (car (find-all-instances-context
;;       '(:l (?x) (and (?x BW-concept-structure.n) (^me choose.v ?x))))))
;;     (request-goal-rep (cdr (get-record-structure concept-name)))
;;     ; NOTE: currently no special offline (terminal mode) procedure
;;     ; for getting goal schema.
;;     (setq goal-schema (get-goal-rep))
;;     (setq goal-name (gensym "BW-goal-rep"))
;;     (add-alias (cons '$ goal-schema) goal-name)
;;     (store-in-context (list goal-name 'goal-schema1.n))
;;     (store-in-context (list goal-name 'instance-of.p concept-name)))
;; ) ; END form-spatial-representation





(defun observe-step-towards-goal (goal-rep)
;`````````````````````````````````````````````
; Observes next step towards the currently active BW goal-schema,
; through reading IO file. Store fact in context.
; TODO: might need changing - see note on 'find4.v' implementation.
; Currently, the (?x step1-toward.p ?goal-rep) step is hard-coded
; (based on the value of ?goal-rep extracted from the lambda function
; in the find4.v action), which it shouldn't be.
;
  (let (step)
    (setq step (if *live* (get-planner-input)
                          (get-planner-input-offline)))
    (remove-from-context `(?x step1-toward.p ,goal-rep))
    (if step
      (store-in-context `(,step step1-toward.p ,goal-rep))))
) ; END observe-step-toward-goal





(defun choose-variable-restrictions (sk-var restrictions)
;``````````````````````````````````````````````````````````
; Handles any indefinite quantification of a variable filled
; through a choice  made by Eta, given a list of restrictions.
; 'sk-var' is the variable to be replaced, e.g., '?c'.
; 'restrictions' may be a lambda abstract, possibly preceded by
; some adjective modifier, e.g.,
; (random.a (:l (?x) (and (?x member-of.p ?cc)
;                         (not (^you understand.v ?x)))))
; The fact that some individual was chosen is also stored in context.
; NOTE: does this make sense? i.e., should something being chosen be
; stored in dialogue context? If the choosing occurs as part of a loop,
; how is something "unchosen" at the end of the loop? Currently I do that
; here, so basically only one (^me choose.v ?x) fact can be active in
; context at once, but it's pretty ugly.
;
  (let (sk-name sk-const lambda-descr modifier candidates)
    (setq sk-const (skolem (implode (cdr (explode sk-var)))))
    ; Allow for initial modifier 
    (when (not (lambda-descr? restrictions))
      (setq modifier (car restrictions)) (setq restrictions (second restrictions)))
    (setq lambda-descr restrictions)
    (setq candidates (find-all-instances-context lambda-descr))
    (format t "given restriction ~a, found candidates ~a~%" lambda-descr candidates) ; DEBUGGING
    (format t "using modifier ~a to choose~%" modifier) ; DEBUGGING
    (setq sk-name (cond
      ((equal modifier 'random.a)
        (nth (random (length candidates)) candidates))
      (t (car candidates))))
    ; Store fact that sk-name chosen in context (removing any existing choice).
    (remove-from-context '(^me choose.v ?x))
    (store-in-context `(^me choose.v ,sk-name))
    sk-name)
) ; END choose-variable-restrictions





(defun obviated-question (sentence eta-action-name)
;````````````````````````````````````````````````````
; Check whether this is a (quoted, bracketed) question.
; If so, check what facts, if any, are stored in gist-kb-user under 
; the 'topic-keys' obtained as the value of that property of
; 'eta-action-name'. If there are such facts, check if they
; seem to provide an answer to the gist-version of the question,
; which will be the last gist clause stored under property
; 'gist-clauses' of 'eta-action-name'.
; NOTE: modified to check if gist clause contains question rather than surface
; sentence (B.K. 4/17/2020)
;
  (let (gist-clauses topic-keys facts)
    ;; (format t "~% ****** input sentence: ~a~%" sentence)
    (setq gist-clauses (get eta-action-name 'gist-clauses))
    ;; (format t "~% ****** gist clauses are ~a **** ~%" gist-clauses)
    ;; (format t "~% ****** quoted question returns ~a **** ~%" (some #'question? gist-clauses)) ; DEBUGGING
    (if (not (some #'question? gist-clauses))
      (return-from obviated-question nil))
    (setq topic-keys (get eta-action-name 'topic-keys))
    ;; (format t "~% ****** topic key is ~a ****** ~%" topic-keys) ; DEBUGGING
    (if (null topic-keys) (return-from obviated-question nil))
    (setq facts (remove nil (mapcar (lambda (key) (gethash key (ds-gist-kb-user *ds*))) topic-keys)))
    ;; (format t "~% ****** gist-kb ~a ****** ~%" (ds-gist-kb-user *ds*))
    ;; (format t "~% ****** list facts about this topic = ~a ****** ~%" facts)
    ;; (format t "~% ****** There is no fact about this topic. ~a ****** ~%" (null facts)) ; DEBUGGING
    (if (null facts) (return-from obviated-question nil))
    ; We have an Eta question, corresponding to which we have stored facts
    ; (as user gist clauses) that seem topically relevant.
    ; NOTE: in this initial version, we don't try to verify that the facts
    ; actually obviate the question, but just assume that they do. 
  facts)
) ; END obviated-question





(defun obviated-action (eta-action-name)
;`````````````````````````````````````````
; Check whether this is an obviated action (such as a schema instantiation),
; i.e. if the action has a topic-key(s) associated, check if any facts are stored
; in gist-kb-user under the topic-key(s). If there are such facts, we assume that
; these facts obviate the action, so the action can be deleted from the plan.
;
  (let (topic-keys facts)
    (setq topic-keys (get eta-action-name 'topic-keys))
    ;; (format t "~% ****** topic key is ~a ****** ~%" topic-keys) ; DEBUGGING
    (if (null topic-keys) (return-from obviated-action nil))
    (setq facts (remove nil (mapcar (lambda (key) (gethash key (ds-gist-kb-user *ds*))) topic-keys)))
    ;; (format t "~% ****** gist-kb ~a ****** ~%" (ds-gist-kb-user *ds*))
    ;; (format t "~% ****** list facts about this topic = ~a ****** ~%" facts)
    ;; (format t "~% ****** There is no fact about this topic. ~a ****** ~%" (null facts)) ; DEBUGGING
    (if (null facts) (return-from obviated-action nil))
  facts)
) ; END obviated-action





(defun form-gist-clauses-from-input (words prior-gist-clause)
;``````````````````````````````````````````````````````````````
; Find a list of gist-clauses corresponding to the user's 'words',
; interpreted in the context of 'prior-gist-clause' (usually a
; question output by the system). Use hierarchically related 
; choice trees for extracting gist clauses.
;
; The gist clause extraction patterns will be similar to the
; ones in the choice packets for reacting to inputs, used in
; the previous version; whereas the choice packets for reacting
; will become simpler, based on the gist clauses extracted from
; the input.
;
; - look for a final question -- either yes-no, starting
;   with auxiliary + "you{r}", or wh-question, starting with
;   a wh-word and with "you{r}" coming within a few words.
;   "What about you" isa fairly common pattern. (Sometimes the
;   wh-word is not detected but "you"/"your" is quite reliable.)
;   The question, by default, is reciprocal to Eta's question.
;
  (let ((n (length words)) tagged-prior-gist-clause tagged-words relevant-trees sentences
        specific-tree thematic-tree facts gist-clauses)

    ; Get the relevant pattern transduction tree given the gist clause of Eta's previous utterance.
    ;````````````````````````````````````````````````````````````````````````````````````````````````
    ;; (format t "~% prior-gist-clause = ~a" prior-gist-clause) ; DEBUGGING
    (setq tagged-prior-gist-clause (mapcar #'tagword prior-gist-clause))
    ;; (format t "~% tagged prior gist clause = ~a" tagged-prior-gist-clause) ; DEBUGGING
    (setq relevant-trees (cdr
      (choose-result-for tagged-prior-gist-clause '*gist-clause-trees-for-input*)))
    ;; (format t "~% this is a clue == ~a" (choose-result-for tagged-prior-gist-clause
    ;;   '*gist-clause-trees-for-input*))
    ;; (format t "~% relevant trees = ~a" relevant-trees) ; DEBUGGING   
    (setq specific-tree (first relevant-trees)) 
    (setq thematic-tree (second relevant-trees))  

    ;; ; Get the list of gist clauses from the user's utterance, using the contextually
    ;; ; relevant pattern transduction tree.
    ;; ;```````````````````````````````````````````````````````````````````````````````````````````````````````
    ;; (setq tagged-words (mapcar #'tagword words))
    ;; ;; (format t "~% tagged words = ~a" tagged-words) ; DEBUGGING
    ;; (setq facts (cdr (choose-result-for tagged-words relevant-tree)))
    ;; (format t "~% gist clauses = ~a" facts) ; DEBUGGING

    ; Split user's reply into sentences for extracting specific gist clauses
    ;`````````````````````````````````````````````````````````````````````````
    (setq sentences (split-sentences words))
    (dolist (sentence sentences)
      (let ((tagged-sentence (mapcar #'tagword sentence)))
        (setq clause (cdr (choose-result-for tagged-sentence specific-tree)))
        (when (atom (car clause)) (setq clause (list clause))) ; in case no topic-key
        (when clause
          (setq keys (second clause))
          (store-gist (car clause) keys (ds-gist-kb-user *ds*))
          (push (car clause) facts))))

    ; Form thematic answer from input (if no specific facts are extracted)
    ;``````````````````````````````````````````````````````````````````````
    (when (and (> (length sentences) 2) (null facts))
      (setq clause (cdr (choose-result-for (mapcar #'tagword words) thematic-tree)))
      (when (atom (car clause)) (setq clause (list clause))) ; in case no topic-key
      (when clause
        (setq keys (second clause))
        (store-gist (car clause) keys (ds-gist-kb-user *ds*))
        (push (car clause) facts)))

    ; The results obtained will be stored as the 'gist-clauses'
    ; property of the name of the user input. So, 'facts' should
    ; be a concatenation of the above results in the order in
    ; which they occur in the user's input; in reacting, Eta will
    ; pay particular attention to the first clause, and any final question.
    (setq gist-clauses (reverse facts))

    ;; (format t "~% extracted gist clauses: ~a" gist-clauses) ; DEBUGGING
	
	  ; Allow arbitrary unexpected inputs to be processed
    ; replace nil with (null gist-clauses)
    (if nil (list words)
		  gist-clauses)
)) ; END form-gist-clauses-from-input





(defun form-ulf-from-clause (clause)
;`````````````````````````````````````
; Find the ULF corresponding to the user's 'clause' (a gist clause).
; **Right now, this uses *spatial-question-ulf-tree* directly, instead of
;   using, say *clause-ulf-tree*, as a general starting point for any
;   sentential input. When *clause-ulf-tree* has been designed 
;   (branching to subtrees for assertions, questions, requests, etc.)
;   it should replace *spatial-question-ulf-tree* below.
;
; **For initial experimentation, the "raw" question rather than any
;   gist clause derived from it is processed. The idea is that we
;   would transform inputs like "What's to the right of it?" or
;   "Add another one" into gist clauses, using the prior utterance
;   or action. This should be possible with the existing gist clause
;   mechanisms. For example, if the prior utterance was "Put a red
;   block on the NVidia block", then "Add another one" should be
;   interpretable as "Put another red block on the current structure",
;   or something like that. The present program would be applied 
;   to this. Cf., the use of the tagged prior gist clause in
;   'form-gist-clauses-from-input'.
;
; Use hierarchical choice trees for extracting the ULF.
;
  (let (tagged-clause ulf)
    (setq tagged-clause (mapcar #'tagword clause))
    (setq ulf (choose-result-for tagged-clause '*clause-ulf-tree*))
  ulf)
) ; END form-ulf-from-clause





(defun form-user-action-type (clause)
;```````````````````````````````````````
; Given a gist clause, find a corresponding 'action type'
; (i.e., a predicate like say-bye.v) using hierarchical
; pattern transduction.
;
  (let (tagged-clause action-type)
    (setq tagged-clause (mapcar #'tagword clause))
    (setq action-type (choose-result-for tagged-clause '*clause-action-type-tree*))
  action-type)
) ; END form-user-action-type





(defun commit-perceptions-to-memory (perceptions user-ulf)
;``````````````````````````````````````````````````````````
; Given perceptions by the system (e.g., block moves) and/or a
; query ULF, store these facts in short-term memory (context).
; The facts are deindexed and stored in context as, e.g.,
; ((you.pro ((past move.v) |B1|)) @ NOW1), though the indexical
; formula is also stored hashed on the time NOW1.
; TODO: as indicated, many aspects of this will need changing.
; I'm also not sure that such historical temporal facts should
; be stored in short-term memory/context at all, versus some
; form of long-term memory.
;
  ; Store move.v facts in context, deindexed at the current time
  ; TODO: COME BACK TO THIS
  ; It seems like this should be somehow an explicit store-in-context step in schema, but which facts are
  ; indexical? Should e.g. past moves in fact be stored in memory rather than context?
  (let ((action-perceptions (remove-if-not #'verb-phrase? perceptions)))
    (when action-perceptions
      (setq *time-prev* *time*)
      (mapcar (lambda (perception)
          (let ((perception1 (list perception '@ *time*)))
            (update-time)
            (store-in-context perception1)
          ))
        action-perceptions)))

  ; Store ULF of user utterance in context, deindexed at the current time
  ; TODO: COME BACK TO THIS
  ; This should probably be done elsewhere (e.g. at the time of Eta processing the say-to.v episode),
  ; but then the utterance would come temporally before any block moves, whereas it should be the other
  ; way around. The perceive-world.v action in general needs to be rethought (since really observing a
  ; user say-to.v action, much like a move.v action or any other action, IS a perceive world action).
  ; Update Eta's current time
  (when user-ulf
    (let ((utterance-prop `((^you ((past ask.v) ,user-ulf)) @ ,*time*)))
      (update-time)
      (store-in-context utterance-prop)
    ))
) ; END commit-perceptions-to-memory





(defun eval-truth-value (wff)
;```````````````````````````````
; Evaluates the truth of a conditional schema action.
; This assumes a CWA, i.e., if something is not found in
; context, it is assumed to be false.
;
  (cond
    ; (wff1 = wff2)
    ((equal-prop? wff)
      (setq wff (eval-functions wff))
      (equal (first wff) (third wff)))
    ; (not wff1)
    ((not-prop? wff)
      (not (eval-truth-value (second wff))))
    ; (wff1 and wff2)
    ((and-prop? wff)
      (and (eval-truth-value (first wff))
           (eval-truth-value (third wff))))
    ; (wff1 or wff2)
    ((or-prop? wff)
      (or  (eval-truth-value (first wff))
           (eval-truth-value (third wff))))
    ; Otherwise, check to see if wff is true in context
    (t (get-from-context wff))
)) ; END eval-truth-value





(defun choose-result-for (tagged-clause rule-node)
;```````````````````````````````````````````````````
; This is just the top-level call to 'choose-result-for', with
; no prior match providing a value of 'parts', i.e., 'parts' = nil;
; this is to enable tracing of just the top-level calls
  (choose-result-for1 tagged-clause nil rule-node)
) ; END choose-result-for





(defun choose-result-for1 (tagged-clause parts rule-node)
;`````````````````````````````````````````````````````````
; This is a generic choice-tree search program, used both for
; (i) finding gist clauses in user inputs (starting with selection
; of appropriate subtrees as a function of Eta's preceding
; question, simplified to a gist clause), and (ii) in selecting
; outputs in response to (the gist clauses extracted from) user 
; inputs. Outputs in the latter case may be verbal responses
; obtained with reassembly rules, or names (possibly with
; arguments) of other choice trees for response selection, or
; the names (possibly with arguments) of schemas for planning 
; an output. The program works in essentially the same way for
; purposes (i) and (ii), but returns
;      (cons <directive keyword> result)
; where the directive keyword (:out, :subtree, :subtree+clause,
; :schema, ...) is the one associated with the rule node that
; provided the final result to the calling program. (The calling
; program is presumed to ensure that the appropriate choice tree
; is supplied  as 'rule-node' argument, and that the result is
; interpreted and used as intended for that choice tree.)
;
; So, given a feature-tagged input clause 'tagged-clause', a list 
; 'parts' of matched parts from application of the superordiate
; decomposition rule (initially, nil), and the choice tree node 
; 'rule-node' in a tree of decomposition/result rules, we generate
; a verbal result or other specified result starting at that rule,
; prefixed with the directive keyword.
;
; Decomposition rules (as opposed to result rules) have no
; 'directive' property (i.e., it is NIL). Note that in general
; a decomposition rule will fail if the pattern it supplies fails
; to match 'tagged-clause', while a result rule will fail if its
; latency requirements prevent its (re)use until more system
; outputs have been generated. (This avoids repetitive outputs.)
;
; Note also that result rules can have siblings, but not children,
; since the "downward" direction in a choice tree corresponds to
; successive refinements of choices. Further, note that if the
; given rule node provides a decomposition rule (as indicated by
; a NIL 'directive' property), then it doesn't make any direct
; use of the 'parts' list supplied to it -- it creates its own
; 'newparts' list via a new pattern match. However, if this
; match fails (or succeeds but the recursion using the children 
; returns NIL), then the given 'parts' list needs to be passed
; to the siblings of the rule node -- which after all may be 
; result rules, in particular reassembly rules.
;
; Method:
;````````
; If the rule has a NIL 'directive' property, then its 'pattern'
; property supplies a decomposition rule. We match this pattern,
; and if successful, recursively seek a result from the children
; of the rule node (which may be result rules or further decomp-
; osition rules), returning the result if it is non-nil; in case
; of failure, we recursively return a result from the siblings
; of the rule node (via the 'next' property); these siblings
; represent alternatives to the current rule node, and as such
; may be either alternative decomposition rules, or result rules 
; (with a non-nil 'directive' property) -- perhaps intended as
; a last resort if the decomposition rules at the current level
; fail.
;
; In all cases of non-nil directives, if the latency requirement
; is not met, i.e., the rule cannot be reused yet, the recursive
; search for a result continues with the siblings of the rule.
;
; If the rule node has directive property :out, then its 'pattern'
; property supplies a reassembly rule. If the latency requirement 
; of the rule is met, the result based on the reassembly rule and
; the 'parts' list is returned (after updating 'time-last-used'). 
; The latency criterion uses the 'latency' property of 'rule-node' 
; jointly with the 'time-last-used' property and the global result 
; count, *count*. 
;
; If the rule node has directive property :subtree, then 'pattern'
; will just be the name of another choice tree. If the latency 
; requirement is met, a result is recursively computed using the
; named choice tree (with the same 'tagged-clause' as input).
; The latency will usually be 0 in this case, i.e., a particular
; choice subtree can usually be used again right away.
;
; If the rule node has directive property :subtree+clause, then
; 'pattern' supplies both the name of another choice tree and
; a reassembly pattern to be used to construct a clause serving
; as input in the continued search (whereas for :subtree the
; recursion continues with the original clause). Again the
; latency will usually be 0.
;
; (June 9/19) If the rule node has directive property :ulf-recur,
; then 'pattern' supplies two reassembly rules, the first of which,
; upon instantiation with 'parts', is a list such as
;  ((*be-ulf-tree* ((is be pres))) 
;   (*np-ulf-tree* (the det def) (Nvidia name corp-name) (block cube obj))
;   (*rel-ulf-tree* (to prep dir loc) (the det def) (left noun loc) (of prep))
;   (*np-ulf-tree* (a det indef) (red adj color) (block cube obj)) 
;   (*end-punc-ulf-tree* (? end-punc ques-punc))),
; ie., a list of sublists of tagged words, with each sublist prefaced by
; the name of a rule tree to be used to produce a ulf for that sublist of
; tagged words. The instantiated reassembly rule is then processed
; further, by successively trying to get a result for each of the rule
; trees named in the sublists; if all succeed, the individual results
; are assembled into an overall ULF, and this is the result returned
; (otherwise, the result is nil -- failure). The second reassembly rule
; provides the right bracketing structure for putting together the
; individual ULFs. Example: ((1 2 (3 4)) 5); result for the above:
;     (((pres be.v) (the.d (|NVidia| block.n)) 
;                   (to_the_left_of.p (a.d (red.a block.n)))) ?)
;
; Other directives (leading to direct return of a successful result
; rather than possible failure, leading to continuing search) are 
; - :subtrees (returning the names of multiple subtrees (e.g., 
;   for extracting different types of gist clauses from a 
;   potentially lengthy user input); 
; - :schema (returning the name of a schema to be instantiated, 
;   where this schema requires no arguments); 
; - :schemas (returning multiple schema names, perhaps as 
;   alternatives); 
; - :schema+args (a schema to be instantiated for the specified 
;   args derived from the given 'tagged-clause'); 
; - :gist (a gist clause extracted from the given 'tagged-clause,
;   plus possibly a list of topic keys for storage);
; - :ulf (June 9/19) (returning a ulf for a phrase simple enough
;   to be directly interpreted);
; - perhaps others will be added, such as :subtrees+clauses or
;   :schemas+args
;
; These cases are all treated uniformly -- a result is returned
; (with the directive) and it is the calling program's responsib-
; ility to use it appropriately. Specifically, if the latency
; requirement is met, the value supplied as 'pattern', instantiated
; with the supplied 'parts', is returned. (Thus integers appearing
; in the value pattern are interpreted as references to parts
; obtained from the prior match.) 
;

  ; First make sure we have the lexical code needed for ULF computation
  (if (not (fboundp 'eval-lexical-ulfs)) (load "eval-lexical-ulfs.lisp"))

  (let (directive pattern newparts newclause new-tagged-clause ulf ulfs result)
    ; Don't use empty choice trees
    (if (null rule-node) (return-from choose-result-for1 nil))

    ; Get directive and pattern from rule node
    (setq directive (get rule-node 'directive))
    (setq pattern (get rule-node 'pattern))

    ; If latency is being enforced, skip rule if it was used too recently
    (when (and directive *use-latency*
            (< *count* (+ (get rule-node 'time-last-used)
                          (get rule-node 'latency))))
      (return-from choose-result-for1
        (choose-result-for1 tagged-clause parts (get rule-node 'next))))

    ;; (format t "~% ***1*** Tagwords = ~a ~%" tagged-clause) ; DEBUGGING
    ;; (format t "~% =====2==== Pattern/output to be matched in rule ~a = ~%  ~a and directive = ~a" rule-node pattern directive) ; DEBUGGING
  
    ; Big conditional statement for dealing with all possible directives.
    ; We first deal with cases requiring further tree-descent (with possible
    ; failure and thus recursive backtracking), no directive (i.e. decomposition
    ; rule), :subtree, :subtree+clause, and :ulf-recur
    (cond
      ;``````````````````
      ; No directive
      ;``````````````````
      ; Look depth-first for more specific match, otherwise try alternatives
      ((null directive)
        (setq newparts (match1 pattern tagged-clause))
        ;; (format t "~% ----3---- new part = ~a ~%" newparts) ; DEBUGGING

        ; Pattern does not match 'tagged-clause', search siblings recursively
        (if (null newparts)
          (return-from choose-result-for1
            (choose-result-for1 tagged-clause parts (get rule-node 'next))))

        ; Pattern matched, try to obtain recursive result from children
        (setq result
          (choose-result-for1 tagged-clause newparts (get rule-node 'children)))

        (if result (return-from choose-result-for1 result)
                   (return-from choose-result-for1
                      (choose-result-for1 tagged-clause parts (get rule-node 'next)))))

      ;`````````````````````
      ; :subtree directive
      ;`````````````````````
      ; Recursively obtain a result from the choice tree specified via its
      ; root name, given as 'pattern'
      ((eq directive :subtree)
        (setf (get rule-node 'time-last-used) *count*)
        (return-from choose-result-for1
          (choose-result-for1 tagged-clause parts pattern)))

      ;````````````````````````````
      ; :subtree+clause directive
      ;````````````````````````````
      ; Similar to :subtree, except that 'pattern' is not simply the root
      ; name of a tree to be searched, but rather a pair of form
      ; (<root name of tree> <reassembly pattern>), indicating that the
      ; reassembly pattern should be used together with 'parts' to reassemble
      ; some portion of 'tagged-clause', whose results should then be used
      ; (after re-tagging) in the recursive search.
      ((eq directive :subtree+clause)
        (setf (get rule-node 'time-last-used) *count*)
        (setq newclause (instance (second pattern) parts))
        (setq new-tagged-clause (mapcar #'tagword newclause))
        (return-from choose-result-for1
          (choose-result-for1 new-tagged-clause nil (car pattern))))

      ;````````````````````````
      ; :ulf-recur directive
      ;````````````````````````
      ; Find the instance of the rule pattern determined by 'parts',
      ; which will be a shallow analysis of a text segment, of the
      ; form described in the initial commentary; try to find results
      ; (ULFs) for the component phrases, and if successful assemble
      ; these into a complete ULF for the input. NB: (first pattern)
      ; supplies the top-level phrasal segments to be further analyzed
      ; (using the ulf rule trees heading each phrasal segment), while
      ; (second pattern) supplies the bracketing structure for the
      ; phrasal ULFs.
      ((eq directive :ulf-recur)
        ; Instantiate shallow analysis
        (setq newclause (instance (first pattern) parts))
        ; Interpret recursive phrases; the car of each nonatomic phrase
        ; either gives the name of the relevant rule tree to use, or it
        ; is 'lex-ulf!'; in the former case we proceed recursively, in
        ; the latter we keep the phrase as-is
        (dolist (phrase newclause)
          (if (or (atom phrase) (eq (car phrase) 'lex-ulf!))
            (setq ulf phrase) ; e.g., ulf of (next to) = next_to.p
            (setq ulf
              (choose-result-for (mapcar #'tagword (cdr phrase)) (car phrase))))
          ; If failure, exit loop
          (when (null ulf)
            (setq ulfs nil)
            (return-from choose-result-for1 nil))
          (push ulf ulfs))
        ; Assemble the (initially reversed) list of phrasal ULFs into a
        ; ULF for the entire input, using the second reassembly rule
        (when ulfs
          (setq n (length ulfs)) ; number of phrasal ULFs
          (setq result (second pattern)) ; bracket structure with indices
          (dolist (ulf ulfs)
            (setq ulf (eval-lexical-ulfs ulf))
            (setq result (subst ulf n result))
            (decf n)))
        (return-from choose-result-for1 result))

      ; Now we deal with cases expected to directly return a result,
      ; not requiring allowance for failure-driven backtracking

      ;`````````````````
      ; :ulf directive
      ;`````````````````
      ; In the case of ULF computation we don't prefix the result with
      ; the directive symbol; this is in contrast with  cases like :out,
      ; :gist, :schema, etc., where the schema executor needs to know
      ; what it's getting back as result for an input, and hence what
      ; to do with it
      ((eq directive :ulf)
        (setq result (instance pattern parts))
        (setq result (eval-lexical-ulfs result))
        (return-from choose-result-for1 result))

      ;```````````````````````
      ; :ulf-coref directive
      ;```````````````````````
      ; Obtains a ulf result using the subtree & input specified in the pattern, and
      ; then resolves the coreferences in the resulting ulf
      ((eq directive :ulf-coref)
        (setq newclause (instance (second pattern) parts))
        (setq new-tagged-clause (mapcar #'tagword newclause))
        (setq result (choose-result-for1 new-tagged-clause nil (car pattern)))
        (if (and result (not (equal (car result) :out)))
          (setq result (coref-ulf result)))
        (return-from choose-result-for1 result))

      ;```````````````````````
      ; :gist-coref directive
      ;```````````````````````
      ; TODO: Implement coreference resolution (gist case)
      ((eq directive :gist-coref)
        (setq result (cons directive (instance pattern parts)))
        (setf (get rule-node 'time-last-used) *count*)
        (setq result (coref-gist result))
        (return-from choose-result-for1 result))

      ;```````````````````````````````
      ; Misc non-recursive directives
      ;```````````````````````````````
      ((member directive '(:out :subtrees :schema :schemas 
                           :schema+args :gist :schema+ulf))
        (setq result (cons directive (instance pattern parts)))
        (setf (get rule-node 'time-last-used) *count*)
        (return-from choose-result-for1 result))

      ; A directive is not recognized
      (t
        (format t "~%*** UNRECOGNIZABLE DIRECTIVE ~s ENCOUNTERED FOR RULE ~s~%    FOR THE FOLLOWING PATTERN AND TAGGED CLAUSE: ~%    ~s,  ~s" directive rule-node pattern tagged-clause))
    )
)) ; END choose-result-for1






