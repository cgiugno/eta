; Nov 25/17
; For the top-level blocks world collaborative schema

; Collaborative problem solving in the blocks world
; `````````````````````````````````````````````` 
; (perhaps named *blocks-collab-schema-1*)
; ``````````````````````````````````````
 (defparameter *blocks-collab-schema-1*
 '(event-schema ?e (?x jointly-solve-blocks-world-problems-with.v ?y) 
  ;``````````````````````````````````````````````````````````````````
   "person ?x asks robot assistant ?y to solve various blocks-world problems"
   ; At present (June 2018) I'm assuming that schemas have just one main
   ; variable, which may be an episode- or object-variable, immediately
   ; followed by the type (characterization) of the episode or object.
   ; Probably episode schemas should be divided into situation-schemas,
   ; event-schemas, and process-schemas.

   :Fixed-roles 
     ; "Fixed" means that the variable that is the argument of the predicate
     ; retains the same value throughout any schema instance; (but this may
     ; be an unnecessary distinction); all are tacit Skolem functions of ?e
     ?x person1.n  ; this means (?x person.n)
     ?y robot1.n  
     ?t table1.n 
     ?bb (plur block1.n)
   :Var-roles 
     ?ka1 (kind1-of.n action1.n) 
     ?goal-rep goal-schema1.n 
     ; We will use !-variables as abbreviations for formulas. 
     ; Thus !x denotes (?x person1.n), !y denotes (?y robot1.n), ..., &
     ; !goal-rep denotes (?goal-rep goal-schema1.n), i.e, the claim that
     ; the corresponding "?-variable" is of the (uniquely) specified type.
   :Static-conds 
     ?s1 (?x at-loc.p ?t)
     ?s2 (?y at-loc.p ?t)
     ; We will also use !s1 as abbreviation of "?s1 is characterized by 
     ; (?x at-loc.p ?t)", i.e., ((?x at-loc.p ?t) ** ?s1), & likewise !s2, etc.
   :Goals(?x) 
     ?g1 (?x want1.v (that ((set-of ?x ?y) collaborate1.v)))
     ?g2 (?y want1.v (that ((set-of ?x ?y) collaborate1.v)))
   :Events 
     ; Elsewhere I've used :Intended-episodes (& maybe :Side-effects)
     ; Originally, I had the following iterative action to locate the
     ; required objects. But awareness of the physical context should
     ; instead be the result of perceptual model updates to *curr-state* 
     ; ?e1 (?y repeat2-until.v 
     ;      ?e1.1 (?y see1.v (set-of ?x ?t ?bb))
     ;      ?e1.2 (seq (?y look-for1.v (set-of ?x ?t ?bb))
     ;                  ; results in internal-model update, if successful
     ;                  ((adv-e (for-dur.p (k (one.a second.n)))) (?y wait1.v))))
     ; I've also deleted this as greeting should be reactive (see below):
     ; ?e2 ((set-of ?x ?y) exchange1.v (k (plur greeting.n)))
     ?e1 (?y say-to.v ?x
             '(OK\, I'm ready to help with tasks involving the blocks
               in front of us))
     ?e2 (:repeat-until
          ?e3 (?e2 finished2.a); holds at all times after ?e2 has been finished;
                              ; so we need to make provision for storing this in
                              ; *context* (for the event name that replaces ?e2)
                              ; 
          ?e4 (?x propose1-to.v ?y ?ka1); some type of action; 
          ?e5 (:if (?ka1 = (ka (do2.v nothing.pro)))
                   (:store-in-context '(?e2 finished2.a)))
                   ; could have additional steps, & at least 1 :else-if segment,
                   ; but :else heads the final sequence of steps:
                   :else ; followed by sequence of events 
                     ?e6 (?y create1-from ?ka1 ?goal-rep)
                     ?e7 (:if (?ka1 easy1-for.a ?y)
                               ?e8 (?y express-consent-to.v ?x) 
                               ?e9 (?y do2.v ?ka1)
                               :else 
                               ; {?ka1 is not trivial}:
                                 ?e10 (:repeat-until
                                   ?e11 ((?goal-rep fulfilled1.a) or 
                                         (?e10 interrupted1.a)); may get interrupts
                                   ?e12 (?y try1.v 
                                           (to (find4.v 
                                                 (some ?s ; an action type
                                                   (?s step1-toward.n ?goal-rep)))))
                                        ; success in "trying" this should lead to
                                        ; storage of '((pair ?y ?e12) successful.a)
                                   ?e13 (:if ((pair ?y ?e12) successful.a)
                                             ?e14 (?y say1-to.v ?x
                                                      (that (?y intend1.v ?s)))
                                             ?e15 (?y do2.v ?s)
                                             :else
                                               ?e16 (?y say1-to.v ?x 
                                                     (that (not (?y able1.a ?ka1))))
                                      )); end of inner repeat-loop
          )); end of outer repeat-loop

     ?e17 ((set-of ?x ?y) exchange1.v (k (plur goodbye.n)))

   :Event-relations ; 'before1.p' means "immediately before"; when allowing 
                    ; for some delay, we use 'before2.p'. We use EL rather
                    ; than keywords, since time relationships can be complex
                    ; and numerical, e.g., "at least 1 minute before ...";
     !w7 (?e1 before1.p ?e2 ?e17); i.e., consecutive episodes
     !w8 (?e8 before1.p ?e9)
     !w9 (?e12 before1 ?e13)
     !w10 (?e14 before1.p ?e15)
     ; Note: the execution sequencing is by default in the order of listing,
     ;   with at least modest gaps allowed (=~ length of the actions). But 
     ;   we can add constraints here (e.g., as done here via 'before1.p').
     ;   However, if a constraint is added involving events ?e', ?e" from 
     ;   the same (sub)sequence of schema events, then *all* constraints
     ;   that apply to events in the sequential span from ?e' to ?e" must
     ;   be specified; in other words, the specified constraints for that
     ;   span override their ordering in the schema.
     ; Note: I think we could also have sentential connectives that imply
     ;   event-sequencing for conjoined wffs; e.g. (phi and-then.cc psi), or
     ;   (phi and-imm.cc psi), or maybe even (phi and.cc (eventually.adv-e psi))
   :Necessities; these apply to roles, static-conds and goals; unity by default;
               ; Meaning: how necessary the conditions are for successful execution.
               ; Recall: for var. ?x of specified type P, !x stands for (?x P);
               ; a pair (!x c) stands for ((that !x) is-necessary-to-degree c).
     (!x .5) (!y .5) (!t .5) (!bb .99) (!ka1 .99) (!goal-rep 1) (!s1 1) (!s2 1)
     (!g1 1) (!g2 1)
   :Certainties
     (!e4 .8); i.e., ((that !e4) certain-to-degree .8); 
             ; **Hmm, but what would ?y do if user ?x fails to supply a goal?
     ; Note that certainties generally are used only for expected events
   end.) 
 ); end of *blocks-collab-schema-1*

; Note that a variable like ?e4 (in the outer loop) will in general be
; instantiated multiple times in an instantiation of the schema in a behavioral
; process. The loop will be unwound, i.e., we form a subplan containing one 
; instance of loop steps, and a final copy of the loop. Since subplans have
; their own hash tables for their variables, we will have unambiguous access
; to the values of the variables in each repetition of the loop. 
;
; For example, instantiating the above schema will give one top-level value
; for ?e2, and the subplan for that instance will (eventually) have values
; for ?e4-?e5 (for the initial iteration), as well as its own value for ?e2,
; which will be the identifier of the rest of the iterative action within the
; subplan. The embedded ?e2 event will again be realized as a (sub)subplan,
; etc. Within each ?e2-subevent, we'll also have an ?e10 subevent realized
; as a subplan for the inner loop, etc.

; Evident requirements to executing this schema:
; `````````````````````````````````````````````
; We need mechanisms that will place relevant facts in the current-state KB:
; - must *assume* that (?x at-loc.p ?t), (?y at-loc.p ?t) (because the 
;   "looking for" ?x, the table ?t, and the blocks comes *after* schema
;   initialization; similarly for the "wanting" to collaborate.
; - adding to the kb that (?y see1.v ...) for whatever it sees; also, if
;   it sees the members of a set, then it should affirmatively evaluate that
;   it sees the set; so we need an introspection algorithm for verifying
;   a set-property that requires only that the members have that property;
; - need a way of detecting when an action that is tried is successful; so
;   the "try" action should place the success or failure "announcement" in
;   memory;
; - if ?x says something about stopping an action (or the entire schema), this
;   should place a statement like (?x terminate1.v ?e4) or (?x terminate1.v ?e)
;   in the curr-state kb.
; TBC -- e.g., mapping action tayles to goal schemas, handling iteration,
;    spontaneous interrupts/inserts, etc.

; Comments (following draft of the above schema):
; ```````````````````````````````````````````````
; My sense is that this is too cut-and-dried -- that we need to combine behavioral
; schemas and event schemas more generally with rule-based modification. After all
; "anything can happen" -- the human collaborator might say or request something
; unexpected, another person might show up, blocks might get added or fall off
; the table, etc. 
;
; So for example, the exchange of greetings might be inserted via rules -- if 
; ?y hasn't seen ?x for a while, expect to exchange greetings. Also, announcing
; readiness to collaborate, and intended next actions, could perhaps be based
; on inserting such verbal actions whenever ?y makes a behavioral choice and
; is in a collaborative, "verbose" state. But if ?x says, "You don't need to
; announce every move", then such insertions should be forestalled. Likewise
; awaiting acknowledgement before proceeding with an unobligated action might 
; depend on being in a "subservient" role.
;
; Could the iteration over successive "problems", and the iterations over
; successive steps in solving a problem, be driven by rule-based insertions
; into the plan as well?
; 
; It occurs to me as well that we might separate expectations from corresponding
; reactions when those expectations are met. For example, when someone asks me
; a question in class, I react by answering -- it's not necessary that the
; expectation to answer already be in my lecture plan, even though I do expect
; some questions. It's just that the question itself engenders the obligation
; to answer. In general, our behavior is of course a mixture of reactive and
; planned or rote behavior. We need to smoothly integrate these. It's worth
; noting that standard dialogue management techniques are essentially reactive:
; dialogue state transitions are driven by rules dictating what to do in response
; to the immediately preceding dialogue event.
;
; See "./blocks-qa-schema.lisp" for a start on a pair of reactive greeting schemas.









