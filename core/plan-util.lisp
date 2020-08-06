;; Aug 5/2020
;; ===========================================================
;;
;; TODO: Write header
;;

(setf *print-circle* t) ; needed to prevent recursive loop when printing plan-step

(defstruct plan
;```````````````````````````````
; contains the following fields:
; plan-name       : the generated name for the plan
; schema-name     : the name of the schema used to generate the plan (if any)
; schema-contents : the contents of the schema used to generate the plan (if any)
; curr-step       : the current (to be processed) step of the plan
; vars            : contains any local variables in plan (potentially inherited)
; subplan-of      : points to a plan-step that the plan is a subplan of
;
  plan-name
  schema-name
  schema-contents
  curr-step
  vars
  subplan-of
) ; END defstruct plan



(defstruct plan-step
;```````````````````````````````
; contains the following fields:
; step-of   : points to the plan structure that this is a step of
; prev-step : points to the previous step in the plan
; next-step : points to the next step in the plan
; ep-name   : episode name of step (gist-clauses, etc. are implicitly attached to ep-name)
; wff       : action formula corresponding to episode name
; subplan   : subplan of step (if any)
;
  step-of
  prev-step
  next-step
  ep-name
  wff
  subplan
) ; END defstruct plan-step



(defun init-plan-from-schema (schema-name args) ;[*]
;````````````````````````````````````````````````
; Given a schema name (e.g. '*eta-schema*), instantiate the
; schema that a plan is to be based on. For non-nil 'args', we
; replace successive variables occurring in the header (excluding
; the episode variable ?e itself) by successive elements of 'args'.
;
; TODO: should ?e be instantiated and (<schema-header> ** E1) be
; stored in context at this point?
;
  (let (plan schema sections)
    ; Make plan structure plan-name corresponding to schema-name
    (setq plan (make-plan))
    (setf (plan-plan-name plan) (gensym "PLAN"))
    (setf (plan-schema-name plan) schema-name)

    ;; (format t "'schema-name' of ~a has been set to ~a~%"
    ;;   (plan-plan-name plan) (plan-schema-name plan)) ; DEBUGGING

    ; Retrieve schema contents from schema-name
    ; (copying so destructive edits can be made)
    (setq schema (copy-tree (eval schema-name)))
    (setf (plan-schema-contents plan) schema)

    ; Return error if schema has no :episodes keyword
    (when (not (find :episodes schema))
      (format t "*** Attempt to form plan ~a from schema ~a with no :episodes keyword"
        (plan-plan-name plan) (plan-schema-name plan))
      (return-from init-plan-from-schema nil))

    ;; (format t "schema used to initialize plan ~a is ~% ~a~%"
    ;;   (plan-plan-name plan) (plan-schema-contents plan)) ; DEBUGGING

    ; Destructively ubstitute the arguments 'args' (if non-nil) for the variables
    ; in the plan/schema header (other than the episode variable)
    (if args (nsubst-schema-args args schema))

    ;; (format t "schema used for plan ~a, with arguments instantiated ~% ~a~%"
    ;;   (plan-plan-name plan) (plan-schema-contents plan)) ; DEBUGGING

    ; Get schema sections. 'sections' is a hash table with schema sections as keys
    (setq sections (get-schema-sections schema))

    ; Process each part of the schema separately
    (process-schema-types       plan (gethash :types sections))
    (process-schema-rigid-conds plan (gethash :rigid-conds sections))
    (process-schema-episodes    plan (gethash :episodes sections))

  plan)
) ; END init-plan-from-schema



(defun init-plan-from-episode-list (episodes) ;[*]
;```````````````````````````````````````````````
; Given a list of episodes, create a plan corresponding to that
; sequence of episodes. Used for creating subplans from certain
; non-primitive actions.
;
  (let (plan)
    ; Make plan structure plan-name
    (setq plan (make-plan))
    (setf (plan-plan-name plan) (gensym "PLAN"))

    ; Remove :episodes keyword (if given)
    (if (equal :episodes (car episodes)) (setq episodes (cdr episodes)))

    ; Process episodes
    (process-schema-episodes plan episodes)

  plan)
) ; END init-plan-from-episode-list



(defun process-schema-types (plan types) ;[-]
;`````````````````````````````````````````
; Add all types to context.
; TODO: This is incomplete and needs to be updated in the future.
; Currently doesn't do anything with the proposition variables e.g. !t1
; TODO: Should typed variables try to find a value right away? Probably not,
; if I'm requiring all variables "local" to a schema to be included in
; the :types section. For now, I've kept this the same as before though.
;
  (dolist (type types)
    (when (not (variable? type))
      ; If typed variable, find value for variable through observation and
      ; substitute in both type and in contents of each schema section.
      (when (variable? (car type))
        (push (car type) (plan-vars plan)))
        ; Get skolem name and replace in schema.
        ;; (setq sk-name (observe-variable-type (car type) (second type))) ; UNDO
        ;; (nsubst sk-name (car type) schema) ; UNDO
        ;; (setq type (subst sk-name (car type) type))) ; UNDO
      ; Store type as fact in context.
      ;; (store-in-context type) ; UNDO
      ))
) ; END process-schema-types



(defun process-schema-rigid-conds (plan rigid-conds) ;[*]
;`````````````````````````````````````````````````````
; Add all rigid-conds to context.
; TODO: This is incomplete and needs to be updated in the future.
; Currently doesn't handle formula variables at all (e.g., for a
; rigid-cond like (?b1 red.a)), or do anything with the proposition
; variables e.g. !r1
;
  (mapcar (lambda (cond) (if (not (variable? cond))
      (store-in-context cond)))
    rigid-conds)
) ; END process-schema-rigid-conds



(defun process-schema-episodes (plan episodes) ;[*]
;```````````````````````````````````````````````
; Converts episodes contents of schema to a linked list
; representing the steps in a plan. Returns the first
; plan step.
;
  (let ((steps (form-step-pairs episodes)) first-step prev-step curr-step)

    ; Give error if first element of first pair isn't episode variable starting with '?'
    (when (not (variable? (caar steps)))
      (format t "*** malformed step ~a while trying to form plan ~a from schema ~a (doesn't begin with episode variable)~%"
        (car steps) (plan-plan-name plan) (plan-schema-name plan))
      (return-from process-schema-episodes nil))

    ; Iterate over steps in episodes list
    (dolist (step steps)

      ; Make plan step structure and set data
      (setq curr-step (make-plan-step))
      (setf (plan-step-step-of curr-step) plan)
      (if (null first-step) (setq first-step curr-step))
      (setf (plan-step-ep-name curr-step) (first step))
      (setf (plan-step-wff curr-step) (second step))
      ; When previous step exists, set bidirectional pointers
      (when prev-step
        (setf (plan-step-prev-step curr-step) prev-step)
        (setf (plan-step-next-step prev-step) curr-step))
      ; Previous step becomes current step
      (setq prev-step curr-step))
    
    ; Set curr-step pointer in plan to first step in plan
    (setf (plan-curr-step plan) first-step)

    ;; (format t "action list of argument-instantiated schema is:~%")
    ;; (print-current-plan-steps plan) ; DEBUGGING

    ; Instantiate first episode in plan
    (instantiate-curr-step plan))
) ; END process-schema-episodes



(defun form-step-pairs (episodes) ;[*]
;`````````````````````````````````
; Groups episodes into a list of (ep-name wff) pairs.
;
  (cond
    ((null episodes) nil)
    (t (cons (list (first episodes) (second episodes))
             (form-step-pairs (cddr episodes)))))
) ; END form-step-pairs



(defun add-subplan-curr-step (plan subplan) ;[*]
;````````````````````````````````````````````
; Given a plan and a subplan, attaches the subplan to the
; currently active step of the plan using the add-subplan function.
; Unless the subplan already has an associated schema name/contents, it
; inherits the schema name/contents of the schema used to create the subplan.
; TODO: should this inherit variables defined in the parent plan at all?
; 
  (add-subplan (plan-curr-step plan) subplan)
  (unless (plan-schema-name subplan)
    (setf (plan-schema-name subplan) (plan-schema-name plan)))
  (unless (plan-schema-contents subplan)
    (setf (plan-schema-contents subplan) (plan-schema-contents plan)))
) ; END add-subplan-curr-step



(defun add-subplan (plan-step subplan) ;[*]
;```````````````````````````````````````````
; Adds a subplan to a given plan-step using bidirectional links.
;
  (setf (plan-step-subplan plan-step) subplan)
  (setf (plan-subplan-of subplan) plan-step)
) ; END add-subplan



(defun advance-plan (plan) ;[*]
;```````````````````````````
; Advances the curr-step in plan to the next step.
;
  ; If no curr-step, return nil
  (if (null (plan-curr-step plan)) (return-from advance-plan nil))

  ;; (format t "current action being advanced in plan ~a: ~%  ~a ~a~%" (plan-plan-name plan)
  ;;   (plan-step-ep-name (plan-curr-step plan)) (plan-step-wff (plan-curr-step plan))) ; DEBUGGING

  ; Set curr-step to the next step in plan
  (setf (plan-curr-step plan)
    (plan-step-next-step (plan-curr-step plan)))

  ; Instantiate the episode variable of the new step and substitute
  (instantiate-curr-step plan)

  ;; (format t "remaining steps in plan ~a after advancing: ~%" (plan-plan-name plan))
  ;; (print-current-plan-steps plan) ; DEBUGGING

) ; END advance-plan



(defun update-plan-state (plan) ;[*]
;`````````````````````````````````
; Updates the state of the plan by advancing any step whose
; subplan has been completed, i.e. has no more steps left.
; 
  (let ((curr-step (plan-curr-step plan)) subplan)

    ; If no curr-step, return nil
    (if (null curr-step) (return-from update-plan-state nil))

    (setq subplan (plan-step-subplan curr-step))
    (cond
      ; If no subplan, do nothing
      ((null subplan) nil)
      ; If subplan forms an infinite loop, break loop and do nothing
      ((and (plan-curr-step subplan) (equal subplan (plan-step-subplan (plan-curr-step subplan))))
        (setf (plan-step-subplan curr-step) nil))
      ; Otherwise, recursively update each subplan
      (t (update-plan-state subplan)
        ; Advance plan once subplan is finished
        (when (null (plan-curr-step subplan))
          ;; (format t "subplan ~a has no curr-step, so advancing plan ~a past step: ~%  ~a ~a ~%"
          ;;   (plan-plan-name subplan) (plan-plan-name plan) (plan-step-ep-name curr-step) (plan-step-wff curr-step)) ; DEBUGGING
          (advance-plan plan)))))
) ; END update-plan-state



(defun instantiate-curr-step (plan) ;[*]
;`````````````````````````````````````
; Instantiates the first episode in the plan, destructively substituting
; the skolemized episode wherever it occurs in the plan. Also attaches
; any properties from the hash tables associated with the episode variable
; in the schema.
;
  (let ((curr-step (plan-curr-step plan)) ep-var ep-name schema-name
        gist-clauses semantics topic-keys)

    ; If no curr-step, return nil
    (if (null curr-step) (return-from instantiate-curr-step nil))
    
    ; Get episode-var (if already instantiated, return nil)
    (setq ep-var (plan-step-ep-name curr-step))
    (if (not (variable? ep-var)) (return-from instantiate-curr-step nil))

    ; Generate a constant for the episode and destructively substitute in plan
    (setq ep-name (episode-name ep-var))
    (nsubst-variable plan ep-name ep-var)

    ;; (format t "action list after substituting ~a for ~a:~%" ep-name ep-var)
    ;; (print-current-plan-steps plan) ; DEBUGGING

    ; Attach wff to episode name (likely not used, but for convenience's sake)
    ; TODO: should (wff ** ep-name) be stored in context at this point?
    (setf (get ep-name 'wff) (plan-step-wff curr-step))
    
    ; In the case of an Eta action, transfer properties from ep-var hash tables
    (setq schema-name (plan-schema-name plan))
    (when (eq '^me (car (plan-step-wff curr-step)))
      ; Gist clauses
      (when (get schema-name 'gist-clauses)
        (setq gist-clauses (gethash ep-var (get schema-name 'gist-clauses)))
        (setf (get ep-name 'gist-clauses) gist-clauses))

      ;; (format t "Gist clauses attached to ~a =~% ~a~%"
      ;;   ep-name (get ep-name 'gist-clauses)) ; DEBUGGING

      ; Semantics
      (when (get schema-name 'semantics)
        (setq semantics (gethash ep-var (get schema-name 'semantics)))
        (setf (get ep-name 'semantics) semantics))

      ;; (format t "Semantics attached to ~a =~% ~a~%"
      ;;   ep-name (get ep-name 'semantics)) ; DEBUGGING

      ; Topic-keys
      (when (get schema-name 'topic-keys)
        (setq topic-keys (gethash ep-var (get schema-name 'topic-keys)))
        (setf (get ep-name 'topic-keys) topic-keys))

      ;; (format t "Semantics attached to ~a =~% ~a~%"
      ;;   ep-name (get ep-name 'semantics)) ; DEBUGGING
  ))
) ; END instantiate-curr-step



(defun find-curr-subplan (plan) ;[*]
;`````````````````````````````````
; Finds the deepest subplan of the given plan (starting with current step)
; with an immediately pending step.
;
  (let ((curr-step (plan-curr-step plan)) subplan)

    ; If no curr-step, return nil
    (if (null curr-step) (return-from find-curr-subplan nil))

    ;; (format t "current step of plan ~a is: ~%  ~a ~a~%" (plan-plan-name plan)
    ;;   (plan-step-ep-name curr-step) (plan-step-wff curr-step)) ; DEBUGGING

    (setq subplan (plan-step-subplan curr-step))
    (cond
      ; If no subplan, next action is top-level
      ((null subplan) plan)
      ; If subplan forms an infinite loop, break loop and return subplan
      ((and (plan-curr-step subplan) (equal subplan (plan-step-subplan (plan-curr-step subplan))))
        (setf (plan-step-subplan curr-step) nil)
        subplan)
      ; If the subplan is fully executed, then remove subplan and return plan
      ((null (plan-curr-step subplan))
        (format t "*** find-curr-subplan arrived at a completed subplan ~a~%"
          (plan-plan-name subplan))
        (setf (plan-step-subplan curr-step) nil)
        plan)
      ; Otherwise, subplan is active, so find deepest subplan recursively
      (t (find-curr-subplan subplan))))
) ; END find-curr-subplan



(defun has-next-step? (plan) ;[*]
;`````````````````````````````
; Returns t if plan has another step, nil otherwise.
;
  (if (and plan (plan-curr-step plan)) t nil)
) ; END has-next-step?



(defun process-next-action (plan) ;[*]
;```````````````````````````````````````
; Executes the immediately pending action in the plan, which is the
; current step of the deepest active subplan.
;
; Successful execution of an action returns a pair of variable bindings
; for any variables instantiated during execution of the action, and a
; newly generated subplan (possibly nil). The appropriate substitutions
; are made in the plan. If a new subplan was obtained, it's added as a
; subplan to the current step; otherwise, the plan is advanced.
;
; TODO: Why not use the names of nonprimitive steps themselves as
; subplan names? Answer: We want to potentially allow for associating
; multiple alternative subplans with a given step (if we do this, we
; should change 'subplan' to 'subplans', which will point to a *list* 
; of subplan names); when one subplan fails, the step may still be
; achievable with an alternative subplan. (For user inputs, different
; subplans represent alternative expectations about user behavior, and
; this eventually opens the door to an AND-OR style of planning, as in
; two-person games.)
;
  (let (curr-step subplan wff ret bindings new-subplan)

    ; Find the next action from the deepest active subplan
    (setq subplan (find-curr-subplan plan))

    ; Current step becomes whatever the current step of subplan is
    (setq curr-step (plan-curr-step subplan))

    ; If no curr-step, return nil
    (if (null curr-step) (return-from process-next-action nil))

    ;; (format t "~%steps of currently due plan ~a are: ~%" (plan-plan-name subplan))
    ;; (print-current-plan-steps subplan) ; DEBUGGING

    (setq wff (plan-step-wff curr-step))

    ; Perform the action corresponding to the current step
    ; Returns a pair of variable bindings obtained by the action,
    ; as well as a subplan generated by the action (possibly nil),
    ; e.g., (((var1 val1) (var2 val2)) PLAN788)
    (setq ret (cond
      ; If Eta action
      ((eq (car wff) '^me)
        (implement-next-eta-action subplan))
      ; If User action
      ((eq (car wff) '^you)
        (observe-next-user-action subplan))
      ; If abstract episode
      (t (implement-next-plan-episode subplan))))

    (setq bindings (first ret))
    (setq new-subplan (second ret))

    ;; (when bindings
    ;;   (format t "variable substitutions to be made: ~%  ~a~%" bindings)) ; DEBUGGING
    ;; (when new-subplan
    ;;   (format t "new subplan generated: ~a~%" (plan-plan-name new-subplan))) ; DEBUGGING

    ; Make all variable substitutions in the plan
    (dolist (binding bindings)
      (nsubst-variable subplan (second binding) (first binding)))

    ; If a new subplan was generated, add it as a subplan to the
    ; current step. Otherwise, advance the current (sub)plan.
    (if new-subplan
      (add-subplan-curr-step subplan new-subplan)
      (advance-plan subplan))
      
  wff)
) ; END process-next-action



(defun nsubst-variable (plan val var) ;[*]
;```````````````````````````````````````
; Traverses the linked list of steps, starting from the current step,
; and destructively substitutes val for var in the ep-name and wff
; of each step (this should also replace them in schema-contents, if
; the plan has any defined).
; TODO: need to think of whether this should also substitute val for var
; in any parent plan containing var in the 'vars' slot of the plan structure.
; I've left the step-of pointer in case this is done in the future.
;
  (let ((curr-step (plan-curr-step plan)))

    ; Iterate through each subsequent step and substitute val for var
    (loop while (not (null curr-step)) do
      (setf (plan-step-ep-name curr-step)
        (subst val var (plan-step-ep-name curr-step)))
      (nsubst val var (plan-step-wff curr-step))
      (setq curr-step (plan-step-next-step curr-step)))
  
    ; If plan has schema-contents, substitute val for var there too
    (nsubst val var (plan-schema-contents plan)))
) ; END nsubst-variable



(defun nsubst-schema-args (args schema) ;[*]
;```````````````````````````````````````
; Substitute the successive arguments in the 'args' list for successive
; variables occurring in the schema or plan header exclusive of the 
; episode variable characterized by the header predication (for 
; episodic headers). In relational schemas, headers are assumed to 
; be simple (infix) predications,
;          (<term> <pred> <term> ... <term>),
; and for event schemas they are of form
;          ((<term> <pred> <term> ... <term>) ** <term>).
; We look for variables among the terms (exclusive of the one following
; "**" in the latter header type), and replace them in succession by
; the members of 'args'.
;
  (let (header predication vars)
    (setq header (second (member :header schema)))
    (if (eq (second header) '**)
      (setq predication (first header)) ; episodic
      (setq predication header)) ; nonepisodic
    (if (atom predication) ; unexpected
      (return-from nsubst-schema-args schema))
    (dolist (x predication)
      (if (variable? x) (push x vars)))
    (when (null vars) ; unexpected
      (format t "@@@ Warning: Attempt to substitute values~%    ~a~%    in header ~a, which has no variables~%"
                args predication)
      (return-from nsubst-schema-args schema))
    (setq vars (reverse vars))
    (cond
      ((> (length args) (length vars))
        (format t "@@@ Warning: More values supplied, viz.,~%    ~a,~%    than header ~a has variables~%"
                  args predication)
        (setq args (butlast args (- (length args) (length vars)))))
      ((< (length args) (length vars))
        (format t "@@@ Warning: Fewer values supplied, viz.,~%    ~a,~%    than header ~a has variables~%"
                  args predication)
        (setq vars (butlast vars (- (length vars) (length args))))))
            
      ; Length of 'args' and 'vars' are equal (or have just been equalized)
    (dotimes (i (length args))
      (nsubst (pop args) (pop vars) schema))

    schema)
) ; END nsubst-schema-args



(defun subst-duplicate-variables (plan episodes) ;[*]
;``````````````````````````````````````````````````
; Substitutes all variables in an episode list with duplicate variables,
; inheriting the gist-clauses, ulf, etc. attached to them in the schema
; used (directly or indirectly) to create the current plan.
;
  (let* ((episode-vars (get-episode-vars episodes))
        (new-episode-vars (mapcar (lambda (episode-var)
          (duplicate-variable plan episode-var)) episode-vars))
        (result episodes))
    (mapcar (lambda (var new-var)
      (setq result (subst new-var var result)))
      episode-vars new-episode-vars)
  result)
) ; END subst-duplicate-variables



(defun duplicate-variable (plan var) ;[*]
;`````````````````````````````````````````````
; Duplicates an episode variable, inheriting the gist-clauses,
; ulf, etc. attached to it in the schema used (directly or
; indirectly) to create the current plan.
;
  (let (new-var schema-name)
    ; Create new episode variable
    (setq new-var
      (intern (format nil "~a" (gentemp (string var)))))
    (setq schema-name (plan-schema-name plan))
    ; Inherit gist-clauses, semantics, and topic keys
    (setf (gethash new-var (get schema-name 'gist-clauses))
      (gethash var (get schema-name 'gist-clauses)))
    (setf (gethash new-var (get schema-name 'semantics))
      (gethash var (get schema-name 'semantics)))
    (setf (gethash new-var (get schema-name 'topic-keys))
      (gethash var (get schema-name 'topic-keys)))
  ; Return new var
  new-var)
) ; END duplicate-variable



(defun get-episode-vars (episodes)
;```````````````````````````````````
; Form a list of all episode vars (in proposition form) from a list of episodes.
;
  (let (var vars)
    (cond
      ; Base case - if episodes is a symbol, return the symbol if it is an action
      ; var, or nil otherwise.
      ((symbolp episodes)
        (if (variable? episodes)
          `(,(if (ep-var? episodes) (intern (format nil "~a" episodes)) episodes)) nil))
      ; Recursive case
      (t
        (remove-duplicates
          (remove nil (mapcan #'get-episode-vars episodes))
          :test #'equal))))
) ; END get-episode-vars



(defun print-current-plan-steps (plan) ;[*]
;``````````````````````````````````````
; Prints each subsequent step in a plan, starting from the current
; step, showing the ep-name and wff of each. Does not include subplans.
; For debugging purposes.
;
  (let ((curr-step (plan-curr-step plan)))

    ; Iterate through each subsequent step and print ep-name and wff
    (loop while (not (null curr-step)) do
      (format t "~a ~a~%" (plan-step-ep-name curr-step) (plan-step-wff curr-step))
      (setq curr-step (plan-step-next-step curr-step))))
) ; END print-current-plan-steps



(defun print-current-plan-status (plan) ;[*]
;````````````````````````````````````````
; Prints the current plan status. Shows ep-name and wff of
; current pending step in plan, as well as the status of
; any subplans of that step. For debugging purposes.
;
  (let ((curr-plan plan) curr-step superstep subplan (cont t))

    ; Print top-level information about plan
    (format t "Status of ~a (schema ~a) "
      (plan-plan-name plan) (plan-schema-name plan))
    (setq superstep (plan-subplan-of plan))
    (if superstep
      (format t "(subplan-of ~a):~%" (plan-step-ep-name superstep))
      (format t "(no superstep):~%"))

    ; Print each current step and repeat for subplan, if any
    (loop while cont do
      (setq curr-step (plan-curr-step curr-plan))
      (when (null curr-step)
        (format t "  No more steps in ~a.~%" (plan-plan-name curr-plan))
        (format t "  --------------------~%")
        (return-from print-current-plan-status nil))
      (format t "  rest of ~a = (~a ~a ...)~%" (plan-plan-name curr-plan)
        (plan-step-ep-name curr-step) (plan-step-wff curr-step))
      (setq subplan (plan-step-subplan curr-step))
      (cond
        (subplan
          (format t "  subplan ~a of ~a:"
            (plan-plan-name subplan) (plan-step-ep-name curr-step))
          (setq curr-plan subplan))
        (t (setq cont nil)))))
) ; END print-current-plan-status

