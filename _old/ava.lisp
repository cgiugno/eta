;; Started Sept 12/02
;; Lisp code for ava advisor (a possible prototype for J. Hu's "nancy")
;; Compare also with ~schubert/agent-research/self-aware-agent/semantha.lisp
;;
;; Note that the main task types that Ava ultimately needs to know how
;; to do, apart from executive control (a fixed queue-based scheme), are
;; assumed to be the following (and I've created stubs for them):
;; - infer immediate consequences of a fact
;; - suggest goals based on a current new fact (along with context and
;;   general knowledge, of course)
;; - incorporate a goal into the current plan if possible
;;   (i.e., if it seems to raise expected net utility)
;; - expand a goal in the current plan (& revise expected net utility)
;; - do an immediately executable action (easy: the code is there);
;;   but this should probably be followed by updating expected net
;;   utility;
;; - record an action of the current plan as 'done', advancing the
;;   now-point in the plan accordingly; (also make inferences about
;;   the result of the completed action?)
;; - utter an assertion, question, suggestion, hello, bye, thanks,
;;   or expression of feeling; 
;; - listen for, and read, new input;
;; - interpret new input, adding the result to the context, and to
;;   the pending facts (to be used for inference and goal generation)
;;
;; May 7/10: One thing that may be missing above is a clearly defined
;; role for goal-directed *reasoning*, including the goal of more fully
;; working out the expected effects, and net utility of the current
;; plan. Rather, I'm emphasizing planning and forward inference.
;; In general, inference should serve the purpose of increasing
;; knowledge about the world (including Ava and the dialog partner),
;; proving action pre-/corequisites/effects true (or false), and
;; answering questions requiring inference. (My notes on 'Semantha'
;; were more concerned with this.)
;;
;; Another issue is analogical inference -- both forward and goal-
;; directed inference are possible using analogies with known facts
;; or clusters of related facts. One could either make analogies
;; directly, or generate more general hypotheses from known 
;; (clusters of) facts, use these for tentative inferences, and
;; use later confirmation or disconfirmation of the inferences
;; to strenghten, weaken, repair, or abandon the hypotheses.

(defstruct ds             ; a discourse state
  dialog                  ; list of analyzed utterances and their interpretations
  curr-input              ; the last user input received
  curr-queue              ; This equals 'pending-facts, 'potential-goals, 
                          ; 'pending-goals, or 'pending-steps, indicating which
                          ; of the task queues was last used for task-retrieval.
  pending-facts           ; a queue of facts awaiting inference/goal triggering 
                          ; *** LOOKING FOR ASTERISKS AND 'TBC' BELOW, IT TURNS
                          ;     OUT I LATER THOUGHT GIVEN/INFERRED FACTS SHOULD
                          ;     BE DIRECTLY ASSERTED AND USED TO GENERATE 
                          ;     FURTHER FACTS (UP TO A POINT) AND POTENTIAL
                          ;     GOALS; BUT I WONDER ABOUT THAT: IF SOME REALLY
                          ;     URGENT GOAL IS INFERRED, SHOULDN'T FORWARD
                          ;     INFERENCE STOP IN FAVOR OF PLANNING & ACTION?
                          ;     I.E., SHOULDN'T FORWARD INFERENCE TRADE OFF
                          ;     AGAINST EVERYTHING ELSE? AN ISSUE HERE IS HOW 
                          ;     FAR WE WANT FORWARD INFERENCING TO GO BEFORE
                          ;     WE CONSIDER SWITCHING TO OTHER TASKS -- JUST 
                          ;     DEPTH 1, PERHAPS, AND PUT THE NEW CONCLUSIONS
                          ;     ON 'pending-facts'?
  pending-facts-metrics   ; count, importance, time metrics for pending-facts
  potential-goals         ; a queue of goals for possible incorporation into plan
  potential-goals-metrics ; count, importance, time metrics for potential-goals
  pending-goals           ; a queue of (nonprimitive) goals awaiting expansion
  pending-goals-metrics   ; count, importance, time metrics for pending-goals
  pending-steps           ; a queue of incremental plan-steps to be carried out
  pending-steps-metrics   ; count, importance, time metrics for pending-steps
  curr-plan               ; the current plan of action
  curr-facts              ; salient, currently true indexical facts
  permanent-facts         ; salient nonindexical knowledge for the current dialog
  kb-name                 ; name of the background KB being used for the dialog
 )
   ; We'll probably want a list of salient referents, a clock time,
   ; and other contextual information as well.

(defvar *ds*);  will become the current discourse state, holding 
             ;  current facts, goals, plan, etc.
(defvar *dss*); a pushdown stack of past discourse states, at the time
              ; the dialogs terminated (and so each dialog field contains
              ; a completed dialog). We put a COPY of *ds* onto *dss* when
              ; a dialog terminates, otherwise the next session obliterates
              ; it. At this point in the system design, there's no provision
              ; for checkpointing before shutting down Lisp, i.e., multiple
              ; dialogs can only be run in the same Lisp session.

(defvar *magic-word* 'go-to-sleep); for "involuntary" termination

(defvar *fact-count* 0); to be incremented and used for fact names
(defvar *goal-count* 0); not referenced yet (as of Sept 11/03)
(defvar *step-count* 0); --- ditto ---

; Some simple utilities: (I'm not sure which ones I'll really need)
;~~~~~~~~~~~~~~~~~~~~~~~
(defun set-dialog (dialog) (setf (ds-dialog *ds*) dialog))

(defun set-pending-facts (facts) (setf (ds-pending-facts *ds*) facts))

(defun push-pending-fact (fact) (push (ds-pending-facts *ds*) fact))

; ... etc. It remains to be seen what's really needed.

; Define the necessary inference catchers 
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; for catching inferences that posit potential goals or potential 
; expansions of a goal
;
; [2011: 'define-catcher' is not an existing function, but one that I
; was apparently planning to implement; apparently I wanted to associate
; a function 'potential-goal-inference' with the global parameter
; '*potential-goal-inferences*', with the idea that the latter would
; accumulate potential goal inferences generated by the function
; 'potential-goal-inference'; similarly for goal expansion. But the
; way 'catch' and 'throw' work in Lisp is that a "throw" is essentially
; like a "return" (of a value), except that it doesn't in general return
; a value for the 'prog' (or other block) containing the return, but
; rather, returns (throws) a value any number of calling levels "upward" 
; in the code, to whatever program contains the key of the 'catch' 
; construct. As in the case of a return, no further code is executed 
; beyond the throw-point. It'll take some thought to figure out whether/how
; catch and throw can be used to accumulate potential goal or goal-
; expansion inferences ...]
(define-catcher '*potential-goal-inferences* #'potential-goal-inference)
(define-catcher '*goal-expansion-inferences* #'goal-expansion-inference)


(defun potential-goal-inference (wff); Feb 11/05
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; Check whether wff is a claim of type "It may be gainful for Ava to
; do x and thereby achieve y", where y may be "respond appropriately to
; speech act z", or "please the user", or whatever.
;
; TBC -- I 1st need to code the knowledge that will yield the kinds
;        of inferences to be caught here (and by the next predicate).
 )

(defun goal-expansion-inference (wff); Feb 11/05
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; Check whether wff is a claim of type "A good way of achieving goal x
; may be to do y" (where y may be a number of steps, i.e., a complex 
; action).
;
; TBC 
 )

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;                                                 ;;
        ;;      M A I N   'A V A'   P R O G R A M          ;;
        ;;                                                 ;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ava (); Sept 12/02; updated Sept 11/03
~~~~~~~~~~~~~
; Top-level controller of the advising agent. It initializes the discourse 
; state *ds* by asserting some initial facts (leading to initialization 
; of the `pending-facts' priority queue), and then runs through a cycle of
; (1) quitting if `do-task' returned 'stop (e.g., because ava just said 
;     goodbye or the *magic-word* was input); or 
; (2) taking an item from the task queues, and carrying out the corresponding 
;     task; tentatively the 4 task queues, `pending-facts', `potential-goals',
;     `pending-goals', and `pending-steps' are accessed cyclically, without
;     preference for one or the other. Eventually we might make access
;     frequency proportional to some metric based on number of tasks,
;     importance of tasks, and estimated execution time of tasks in each
;     queue. When terminating, the discourse state *ds* is pushed onto *dss*.
; 
; Ava can be peremptorily terminated through input of the *magic-word*,
; which is checked for before any attempt at interpretation is made.
; Currently there is no provision for resumption of the same dialog
; after stopping; but we can start a new one, where Ava will no longer
; know who the user is, or what was said in the previous dialogue. But
; it would be straightforward to pop *ds* from *dss* in order to resume,
; at the same time storing some assertions about the hiatus.
;
 (prog  (diagnostics-outstream fact-name1 fact-name2 qid+task-name result input)
        (setq *ds* (make-ds)); initialize to empty discourse state
        ;
        ; We want to avoid having Ava spill all its commentary to the user,
        ; so we set lib::*print-to* to an output stream that goes to an
        ; appropriate file:
        (setq diagnostics-outstream
           (open "diagnostics-outfile" :direction :output
               :if-does-not-exist :create :if-exists :overwrite))
        (setq lib::*print-to* diagnostics-outstream)
        ; (We will close diagnostics-outstream at the end.)
        ; 
        (setq fact-name1 
            (create-fact 
               '(:i User wants (:f That (:i (:f Coll Ava User) converse))) ))
        (setq fact-name2
            (create-fact
               '(:i (:f Coll Ava User) able (:f Ka converse)) ))
        ; Remark: These 2 facts could themselves be made a matter of
        ; inference. The only "incontrovertible" fact is that "the lines
        ; of communication are open", with some person at the user end
        ; attending to that line of communication. From this it is very
        ; *likely* that the user wants to talk. For the second fact we
        ; could have as basis that Ava is "awake", from which the ability
        ; to converse follows.
     ;  **** TBC I HAVEN'T CHANGED MUCH YET BUT WANT TO CHANGE THE FOLLOWING
     ;  (AND ALL OTHER FACT STORAGE ACTIONS) TO DIRECT 'store' CALLS;
     ;  INFERENCE WILL NO LONGER BE VIA PENDING-FACTS, BUT BY DIRECT STORAGE
        (assert-curr-fact fact-name1 10 1.0); interest level (max 10), prob.
        (assert-curr-fact fact-name2 1 1.0)
  
        ; Check if there are any pending tasks. If so, do the first task,
        ; and if this returns 'stop' then save the discourse and return 
        ; from Ava, otherwise go on to the next task. Ava should always
        ; be adding new 'listen' tasks to its pending goals after saying 
        ; something, so it shouldn't run out of tasks before encountering
        ; 'stop'.
        ;
    act (setq qid+task-name (select-and-remove-task))
        (when qid+task-name
              (setq result (do-task qid+task-name))
              (case result 
                    (stop (push *ds* *dss*)
                          ; **** THE ABOVE SHOULD BE CHANGED TO *DUPLICATE* *DS*!
                          (close diagnostics-outstream)
                          (return-from ava '--------------------) )
                    (t (go act)) ))

        ; Unexpected condition: no further tasks. Normally termination
        ; will be due to do-task returning 'stop (after ava says goodbye)
        (close diagnostics-outstream)
        (format t "~%*** AVA terminated, for lack of futher tasks")
        (return-from ava '--------------------) 
 )); end of ava


(defun create-fact (wff); Sept 11/03 (tested)
;~~~~~~~~~~~~~~~~~~~~~~~~
; Create a name for the given formula, by updating the *fact-count* and
; using it as a suffix to "fact". Place the wff under indicator 'wff
; on the property list of the name, and return the name.
 (let (name)
      (incf *fact-count* )
      (setq name (intern (format nil "FACT~a" *fact-count*)))
      (setf (get name 'wff) wff)
      name 
 )); end of create-fact


(defun assert-curr-fact (fact-name interest prob); updated Sept 11/03
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; Make the given fact a current fact in discourse state *ds*, and
; generate the task of reacting to this fact, placing this in the
; tasks queue of *ds*. Before doing this, add a time-stamp to 'fact-name'.
;
; ** I think we may also need to store the fact in the general KB
;    (retracting it when it becomes false or the dialog terminates),
;    so that all facts (whether current or from the kb) can be used 
;    uniformly for inference. Maybe we don't even have a separate 
;    store for salient facts, just biased access to them (or salience-
;    biased selection from accessed facts).
; 
 (let ()
      (setf (get fact-name 'interest) interest)
      (setf (get fact-name 'prob) prob)
      (setf (get fact-name 'time-stamp) (curr-time))
      (push fact-name (ds-curr-facts *ds*)); ** Maybe instead of just pushing on-
                                 ; to a list, we should enter it into hash table,
                                 ; based on content?
      (enqueue-curr-fact fact-name)
 )); end of assert-curr-fact


(defun curr-time (); Sept 11/03 (tested)
;~~~~~~~~~~~~~~~~~~
; Return a representation of the current time, currently as a six-tuple
; of (year month day hour minute seconds-and-fractions), but maybe as
; something like (+ Start seconds), where Start is a constant for 
; the start time of the conversation. (Or we could pick a start constant
; referring to midnight last night, and count in seconds from there...)
; 
 (let ((time-tuple (multiple-value-list (get-decoded-time))))
      (list (nth 5 time-tuple)  ; year
            (nth 4 time-tuple)  ; month
            (nth 3 time-tuple)  ; day
            (third time-tuple)  ; hour
            (second time-tuple) ; minute
            (first time-tuple) ); seconds
 )); end of curr-time


(defun enqueue-curr-fact (fact-name) ; Sept 21/02; updated Sept 11/03
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; Do a quick preliminary evaluation of the given fact to determine
; the priority of triggering inferences and goals from this fact, 
; and then place it in the appropriate location in the pending-facts
; queue of *ds*. ** CURRENTLY JUST ADDS THE FACT AT THE END.
;
; fact-name: In general the name of an indexical wff, the latter
;    stored under indicator 'wff. Its interest, prob, and time-stamp
;    are also on the property list. The "fact" is presumed to have been
;    obtained as part of the interpretation of an input, or as an 
;    inference from other facts.
; The pending-facts queue is straightforwardly represented as a 
;    dotted pair whose car points to the list of pending facts
;    (i.e., to the 1st fact name on the list) and whose cdr points 
;    to the tail of the list (i.e., to the list containing the last 
;    fact name on the pending-facts list).
 (let ((q (ds-pending-facts *ds*)) (tail (list fact-name)))
      (if (null q)
          (setf (ds-pending-facts *ds*) (cons tail tail)) 
          (prog2 (rplacd (cdr q) tail) (rplacd q tail)) ) 
 )); end of enqueue-curr-fact


(defun select-and-remove-task (); Sept 21/02
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; If last-q was the last task-queue from which a selection was made, use
; the "next" queue if possible to select and remove a task (or rather,
; a queue-id plus task name). 
;
; The ordering for moving to the next queue is circular: 
; nil (start) --> pending-facts --> potential-goals --> pending-goals --> 
; pending-steps --> pending-facts --> etc.
;
; If the next queue is empty, try the one after that in the ordering, and
; so on, till last-q is reached again. Dequeue and return the foremost task
; of the first nonempty queue reached in this way, or nil if all are empty.
;
; ** A MORE SOPHISTICATED SELECTION METHOD, USING QUEUE METRICS,
;    MAY BE NEEDED EVENTUALLY
;
 (prog ((last-q (ds-curr-queue *ds*)) next-q)
  next (setq next-q
             (case last-q (pending-facts 'potential-goals)
                          (potential-goals 'pending-goals)
                          (pending-goals 'pending-steps)
                          (T 'pending-facts) ))
       (cond (next-q (setf (ds-curr-queue *ds*) next-q) 
                     (return (remove-task next-q)) )
             ((eq next-q last-q) (return nil))
             (T (go next)) )
 )); end of select-and-remove-task


(defun remove-task (queue-id); Sept 21/02
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; Remove & return the foremost task (more exactly, a queue-id and task name)
; from the queue indicated by the atom `queue-id' (= 'pending-facts, 
; 'potential-goals, 'pending-goals, or 'pending-steps). If the queue is 
; empty return nil.
;
 (prog (q qid+task-name)
       (setq q (case queue-id (pending-facts (ds-pending-facts *ds*))
                              (potential-goals (ds-potential-goals *ds*))
                              (pending-goals (ds-pending-goals *ds*))
                              (T (ds-pending-steps *ds*)) ))
       (if (null q) (return nil))
       (setq qid+task-name (list queue-id (caar q)))
                              ; the queue-id will serve to identify the type of
                              ; task to be done (trigger inferences or goals from
                              ; a fact, embed a potential goal in the plan, etc.)
       (if (eq (car q) (cdr q)); single element? Reset queue to nil
           (setf (case queue-id (pending-facts (ds-pending-facts *ds*))
                                (potential-goals (ds-potential-goals *ds*))
                                (pending-goals (ds-pending-goals *ds*))
                                (T (ds-pending-steps *ds*)) ) nil )
           (rplaca q (cdar q)) ); > 1 element: dequeue 1st element
       (return qid+task-name)
 )); end of remove-task
              
 

(defun do-task (qid+task-name);
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; Carry out the task -- reacting to a fact (by inferring and asserting
; immediate consequences, and suggesting possible goals, including
; goals of explanation), incorporating a possible goal into the plan,
; expanding a goal into a subplan, executing a step of a plan (including 
; listening to the next user turn, verbalizing a set of wffs for 
; output), etc. 
;
; qid+task-name: a pair of form (queue-id name), where queue-id is one of
;       'pending-facts, 'potential-goals, 'pending-goals, or 'pending-
;       steps, and thereby indicates the queue from which the task
;       was taken and hence the type of the task, and name is the name
;       of an appropriate expression -- a current (indexical) fact, a goal, 
;       etc.
;
; All the necessary changes to the discourse state *ds* are made,
; as appropriate for each type of task; in particular if the task is 
; to produce output, this is done and the output & its meaning are
; recorded as part of the dialog
;
; -- I'm not quite clear on the details yet.
; One point to remember: if the task is the step of listening to the
; user, (Ka listen), then (listen) should be called and its value 
; ('stop or nil) returned as value of do-task.
;
; Return 'stop or nil, depending on whether the task was to terminate
; the session (given as a pending step), or some other task like an 
; inference, etc.
;
 (let ((queue-id (first qid+task-name)) (name (second qid+task-name)))
      (case queue-id
            (pending-facts ; trigger inferences and potential goals
               (infer-facts name)
               (suggest-goals name) ); Note: result returned is nil
            (potential-goals ; try to incorporate into curr. plan
               (incorporate-goal name) ); result returned is nil
            (pending-goals ; try to expand into 1 or more alternative 
                           ; subplans. We use the name of the goal to find
                           ; in the plan and expand it there
               (expand-goal name) ); use plan-retrieval if possible,
                                   ; plan synthesis if necessary
            (pending-steps ; carry out the step of speaking or listening
               (do-action name) )); will return 'stop or nil
 )); end of do-task


(defun infer-facts (curr-fact-name); Sept 24/02
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; `curr-fact-name' is the name of a fact, with properties
;  wff, time-stamp, interest, and prob.
;
; Make immediate inferences from the (newly obtained) current fact.
;
; Do this by matching the wff to formulas in curr-facts,
; permanent-facts, and in the KB named by kb-name (all via *ds*),
; and seeing if a nondisjunctive inference can be made by using
; additional facts from these same places. (Maybe allow for binary
; disjunctive conclusion in some cases, if the conclusion is 
; sufficiently interesting.) Use routines similar to those in 
; STORY-INFER.LISP.
;
; In assigning an interest level to a conclusion, let the conclusion
; inherit part of the interest of curr-fact-name (say, half of it -- a 
; tweakable multiplier affecting depth of inference), besides adding
; its intrinsic interest; in this way consequences of interesting facts
; are preferred, regardless of their content. Likewise, the probability
; of a conclusion depends both on the probability of the premises on
; which it is based and on the reliability of the "rules" (e.g.,
; generic facts) that may be involved. 
;
; Annotate the immediate inferences with their interest and probability,
; and assert those whose interest/probability is sufficiently high,
; using assert-curr-fact.
;
; TBC
 )


(defun suggest-goals (curr-fact-name)
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; Try to trigger goal-generation rules using the (newly obtained) 
; current fact stored as 'wff property of curr-fact-name; e.g., there 
; might be a rule that if the user wants a certain proposition, then 
; a suggested goal is to achieve that this proposition holds (over 
; some episode starting as early as possible?)
;
; Index to candidate rules via the wff, match to the appropriate
; literal(s), and try to verify any remaining rule literals needed
; to posit the goal. The suggested goals should be annotated with
; a deadline and an importance rating.
;
; For an initial rough cut, I need to get a suggested goal of conversing
; out of (:i User wants (:f That (:i (:f Coll Ava User) converse))).
; This should be easy, based on a rule matching (:i User wants ?X);
; though there are tricky issues about time and indexicality -- for what
; TIME is this goal suggested? (Right now -- but how do we know that?
; Is it a delayable goal in principle? Should we use "ASAP" -- what
; would it mean?)
; 
; TBC -- For starters, let the fact "User wants to converse with Ava"
;        suggest the plan snippet "converse with user with the goal
;        (potential result?) of increasing the user's and my satisfaction";
;        we then want to try to incorporate this into the current (near-
;        empty initial) plan.
;
 (prog (curr-fact)
       (setq curr-fact (get curr-fact-name 'wff))
 ; TBC ... actually, I can't expect to directly compute the potential
 ;         goals generated by a new (input-derived) fact; they are
 ;         generated indirectly from the assertion of (input-derived)
 ;         facts to EPILOG, using rules that suggest goals (plan snippets).
 ;         What we need instead is a way of "catching" suggested goals as
 ;         they are generated, and putting them on the potential-goals queue.
 ;         It may be best to directly modify EPILOG so that when it stores
 ;         an inferred proposition, it also applies tests to it that have
 ;         been posted at some fixed global site, and lists the propositions
 ;         meeting the tests at corresponding sites; it's a way of "noticing"
 ;         certain kinds of inferences. AVA can then take its proposed goals
 ;         (and also proposed goal decompositions, when elaborating a goal
 ;         from pending-goals) from such a site.

 )


(defun incorporate-goal (goal-name)
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; The suggested goal is given as value of the 'wff property of goal-name.
; Try to incorporate the suggested goal into the current plan, by trying
; to place it at the earliest possible time where it does not disrupt,
; and is not disrupted by, other steps and meets its deadline, and 
; evaluating whether the net utility (or merit, or whatever) of the 
; plan goes up. If unable to do this, perhaps keep it as a tentative 
; step that remains unordered w.r.t. the others for the time being?
; TBC
 )


(defun expand-goal (goal-name)
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; Find methods suitable for achieving the given goal, using the form 
; of the goal to index to relevant methods, and trying to verify any
; remaining literals expressing constraints on the applicability of
; the methods retrieved. [Nov 19/02: Having done the expansion, I
; think we need to check for clobbering interactions, between
; the effects of the steps added and pre-/corequisites of other
; actions/activities. My intuition is that a natural and simple
; strategy (even if not optimal for automated planners) would be
; to immediately resolve clobbering conflicts by (re-)ordering.
; (As long as there are no conflicts, we don't need to order; also,
; I think we should probably discount "potential" clobbering, that
; depends on choices of as-yet-unbound variables.). If we can't find
; a workable ordering, we might just abandon some goals... preferably
; ones not too important.
; TBC
 )


(defun do-action (step-name); Sept 23/02
;~~~~~~~~~~~~~~~~~~~~~~~~~~~
; Carry out the type of listening or speaking (or other??) 
; action given as value of the 'type property of step-name, and
; presumed to be part of and at the frontier of) the current plan.
; This can be done by a Lisp eval call, since steps (as opposed to
; goals) are assumed to be immediately executable (i.e., "primitive" --
; though not necessarily low-level or simple!) Then update the plan
; (at the point identified by step-name) appropriately, and return
; whatever the Lisp call returns (viz., 'stop if the step is (Ka stop)
; or the step type is erroneous, and nil otherwise).
;
 (let ((step (get step-name 'type)) ; e.g., (Ka (decl (That (:i Ava understand
                                    ;                          curr-input ))))
                                    ; e.g., (Ka listen)
                                    ; Besides `decl', we allow `ask', 'suggest',
                                    ; `demand', `say-hi', `say-bye', `thank-for',
                                    ; & `signal' (joy, surprise, disappointment,
                                    ; puzzlement, agreement, etc.)
       result )
      (when (not (eq (car step) 'Ka))
            (format t 
              "~%### improper DO-ACTION arg ~a, should be of form (KA ...)"
               step )
            (return-from do-action 'stop) )
      (setq result (eval (second step)))
      (record-as-done step-name result); the value of this is returned
 )); end of do-action


(defun record-as-done (step-name result); Sept 11/03
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; Remove the named step from the frontier of the current plan, recording
; it in the current context as an executed action. 'result' is the result 
; returned by the Lisp program that executed the step, and may be used in
; miscellaneous ways in updating the context. In particular, I'm thinking
; that there might be special routines for generating logical descriptions
; of "what happened", based on 'result', which might then be asserted as
; events (and might trigger new goals, etc.) much like assertions about
; what the user said. In any case, this program should return 'stop iff
; step-name names the action of terminating the conversation (which we
; could incorporate into saying good-bye, but I'm inclined not to, as 
; the final "good bye" may come either from the user or the system, or
; there may be some other reason for terminating the conversation).
; TBC
 )


(defun decl (prop); Sept 24/02
;~~~~~~~~~~~~~~~~~
; Assert the given proposition in English (i.e., in NLG terminology, this
; is the "realization" step in the generation process. This is done by
; "contextualizing" the wff `prop', i.e., de-scoping quantifiers, replacing 
; arguments by appropriate referring expressions (incl. indexicals and
; pronouns), collapsing conjunctions/disjunctions (incl. moving some
; predications into pre-/postnominal position or pre-/post-VP position, 
; reordering remaining conjuncts if appropriate, etc.; then repositioning 
; adverbs, etc., to mimic English surface order, and finally deleting 
; brackets, adv-/attr-/nn-operators and the like, and replacing remaining 
; atoms by appropriate English word forms.
;
; ** I think I'll need to provide something like presuppositions along
;    with the prop argument, since output form and emphasis (e.g.,
;    use of topicalization or it-clefts) depend on this. Or will the
;    current context "automatically" supply this?
; TBC
 )


(defun ask (ques); Sept 24/02
;~~~~~~~~~~~~~~~~
; Ask the given question in English. This should call most of the same
; subroutines as 'decl'
; TBC
 )


(defun suggest (prop); Sept 24/02
;~~~~~~~~~~~~~~~~~~~~~~
; Suggest the action (given as a proposition) in English; e.g.,
; (That (:i (coll User Ava) talk-about User)) --> "Let's talk about you"
; cf. `decl', `ask'.
; TBC
 )


(defun demand (prop); Sept 24/02
;~~~~~~~~~~~~~~~~~~~~~~
; Demand the action (given as a proposition) in English; e.g.,
; (That (:i User (stop (Ka (prog swear)))) --> "Stop swearing, please"
; cf. `decl', `ask'.
; TBC
 )


(defun say-hi (); Sept 24/02
;~~~~~~~~~~~~~~~
; Greet the user;
; TBC
 )


(defun say-bye (); Sept 24/02
;~~~~~~~~~~~~~~~
; Take leave of the user
; TBC
 )


(defun thank-for (action); Sept 24/02
;~~~~~~~~~~~~~~~~~~~~~~~~
; Thank for some favorable action, like engaging in conversation
; TBC
 )


(defun signal (feeling); Sept 24/02
;~~~~~~~~~~~~~~~~~~~~~~~
; Signal the given feeling, sentiment or reaction with an English
; interjection like "oh", "bravo", "hurrah", "oh-oh", "yikes", "huh",
; "uh-huh", "damn", etc.
; TBC
 )


(defun listen (); Sept 23/02
;~~~~~~~~~~~~~~~
; Read and interpret an input, appropriately updating curr-input and
; dialog in *ds*. If the input starts with the *magic-word*, immediately 
; return 'stop. ** It's assumed here that the input is a list of atoms.
; 
 (let ((input (read-input)))
      (setf (ds-curr-input *ds*) input)
      (when (eq (car input) *magic-word*)
            (format t "~%*** AVA terminated via *magic-word*")
            (return-from listen 'stop) )
      (interpret input); leads to updating of dialog, and assertion of 
                       ; current facts --> new tasks in the task queue
 )); end of listen


(defun read-input ();
;~~~~~~~~~~~~~~~~~~~
; Read and return a complete user input, as a list of words, much as 
; in my pref-parser program;
; TBC
 )


(defun interpret (input);
;~~~~~~~~~~~~~~~~~~~~~~~
; 
; Parse the given input.
; Form the interpretation (a set of facts about what the speaker
; said) and assert the resulting facts; also push the parsed input
; together with its interpretation onto (ds-dialog *ds*). In this
; way we form a record of the entire parsed, interpreted dialog,
; assuming that the output routines also place their output,
; with syntax and meaning representation, in (ds-dialog *ds*).
; TBC
 )
