;; Dec 9/19
;; ================================================
;;
;; Functions used in answering historical questions
;;

; NOW0
; "Where is the Target block?"
; ((|Target| at-loc.p ($ loc 0 0 0)) (|Starbucks| at-loc.p ($ loc 1 0 0)) (|Twitter| at-loc.p ($ loc 2 0 0)) (|Texaco| at-loc.p ($ loc 3 0 0)) (|McDonalds| at-loc.p ($ loc 4 0 0)) (|Mercedes| at-loc.p ($ loc 5 0 0)) (|Toyota| at-loc.p ($ loc 6 0 0)) (|Burger King| at-loc.p ($ loc 7 0 0))  )
; NOW0
; [ans]
; *Moves Starbucks, Target blocks*
; "Where is the Target block?"
; ((|Target| at-loc.p ($ loc 5 5 1)) (|Starbucks| at-loc.p ($ loc 5 5 0)) (|Twitter| at-loc.p ($ loc 2 0 0)) (|Texaco| at-loc.p ($ loc 3 0 0)) (|McDonalds| at-loc.p ($ loc 4 0 0)) (|Mercedes| at-loc.p ($ loc 5 0 0)) (|Toyota| at-loc.p ($ loc 6 0 0)) (|Burger King| at-loc.p ($ loc 7 0 0))      (|Starbucks| ((past move.v) (from.p-arg ($ loc 1 0 0)) (to.p-arg ($ loc 5 5 0)))) (|Target| ((past move.v) (from.p-arg ($ loc 0 0 0)) (to.p-arg ($ loc 5 5 1)))))
; NOW2
; [ans]
; *Moves Target, Twitter blocks*
; "Where was the Target block before I moved it?"
; ((|Target| at-loc.p ($ loc 2 0 1)) (|Starbucks| at-loc.p ($ loc 5 5 0)) (|Twitter| at-loc.p ($ loc 5 5 1)) (|Texaco| at-loc.p ($ loc 3 0 0)) (|McDonalds| at-loc.p ($ loc 4 0 0)) (|Mercedes| at-loc.p ($ loc 5 0 0)) (|Toyota| at-loc.p ($ loc 6 0 0)) (|Burger King| at-loc.p ($ loc 7 0 0))      (|Target| ((past move.v) (from.p-arg ($ loc 5 5 1)) (to.p-arg ($ loc 2 0 1)))) (|Twitter| ((past move.v) (from.p-arg ($ loc 2 0 0)) (to.p-arg ($ loc 5 5 1)))))
; NOW4
; [ans]

; "What blocks did I move?"
; "What blocks did I move since the beginning?"
; "What block did I just move?"

; "Where did I move the Texaco block" => "Where was the Texaco block after I moved it"

; '((|Target| at-loc.p ($ loc 2 0 1)) (|Starbucks| at-loc.p ($ loc 5 5 0)) (|Twitter| at-loc.p ($ loc 5 5 1)) (|Texaco| at-loc.p ($ loc 3 0 0)) (|McDonalds| at-loc.p ($ loc 4 0 0)) (|Mercedes| at-loc.p ($ loc 5 0 0)) (|Toyota| at-loc.p ($ loc 6 0 0)) (|Burger King| at-loc.p ($ loc 7 0 0)))
; '((SUB (AT.P (WHAT.D PLACE.N)) ((THE.D (|Twitter| BLOCK.N)) ((PAST BE.V) *H (ADV-E (BEFORE.P (KE (I.PRO ((PAST MOVE.V) (THE.D (|Twitter| BLOCK.N)))))))))) ?)
; '((SUB (AT.P (WHAT.D PLACE.N)) ((THE.D (|Target| BLOCK.N)) ((PAST BE.V) *H (ADV-E (BEFORE.P (KE (I.PRO ((PAST MOVE.V) (THE.D (|Target| BLOCK.N)))))))))) ?)
; '((SUB (AT.P (WHAT.D PLACE.N)) ((THE.D (|Target| BLOCK.N)) ((PAST BE.V) *H (ADV-E (BEFORE.P (KE ((THE.D (|Target| BLOCK.N)) ((PAST BE.V) (ON.P (THE.D (|Starbucks| BLOCK.N))))))))))) ?)


;
; ((|Target| at-loc.p ($ loc 0 0 0)) (|Starbucks| at-loc.p ($ loc 1 0 0)) (|Twitter| at-loc.p ($ loc 2 0 0)) (|Texaco| at-loc.p ($ loc 3 0 0)) (|McDonalds| at-loc.p ($ loc 4 0 0)) (|Mercedes| at-loc.p ($ loc 5 0 0)) (|Toyota| at-loc.p ($ loc 6 0 0)) (|Burger King| at-loc.p ($ loc 7 0 0))  )
; ((|Target| at-loc.p ($ loc 0 0 0)) (|Starbucks| at-loc.p ($ loc 0 2 0)) (|Twitter| at-loc.p ($ loc 2 0 0)) (|Texaco| at-loc.p ($ loc 3 0 0)) (|McDonalds| at-loc.p ($ loc 4 0 0)) (|Mercedes| at-loc.p ($ loc 5 0 0)) (|Toyota| at-loc.p ($ loc 6 0 0)) (|Burger King| at-loc.p ($ loc 7 0 0))      (|Starbucks| ((past move.v) (from.p-arg ($ loc 1 0 0)) (to.p-arg ($ loc 0 2 0))))  )
; ((|Target| at-loc.p ($ loc 0 0 0)) (|Starbucks| at-loc.p ($ loc 0 2 0)) (|Twitter| at-loc.p ($ loc 2 0 0)) (|Texaco| at-loc.p ($ loc 3 0 0)) (|McDonalds| at-loc.p ($ loc 4 0 0)) (|Mercedes| at-loc.p ($ loc 5 0 0)) (|Toyota| at-loc.p ($ loc 6 0 0)) (|Burger King| at-loc.p ($ loc 7 0 0))  )
; 
; ((|Target| at-loc.p ($ loc 0 0 0)) (|Starbucks| at-loc.p ($ loc 1 0 0)) (|Twitter| at-loc.p ($ loc 2 0 0)) (|Texaco| at-loc.p ($ loc 3 0 0)) (|McDonalds| at-loc.p ($ loc 4 0 0)) (|Mercedes| at-loc.p ($ loc 5 0 0)) (|Toyota| at-loc.p ($ loc 6 0 0)) (|Burger King| at-loc.p ($ loc 7 0 0))  )
; ((|Target| at-loc.p ($ loc 0 0 0)) (|Starbucks| at-loc.p ($ loc 0 2 0)) (|Twitter| at-loc.p ($ loc 2 0 0)) (|Texaco| at-loc.p ($ loc 3 0 0)) (|McDonalds| at-loc.p ($ loc 4 0 0)) (|Mercedes| at-loc.p ($ loc 5 0 0)) (|Toyota| at-loc.p ($ loc 6 0 0)) (|Burger King| at-loc.p ($ loc 7 0 0))      (|Starbucks| ((past move.v) (from.p-arg ($ loc 1 0 0)) (to.p-arg ($ loc 0 2 0))))  )
; ((|Target| at-loc.p ($ loc 0 0 0)) (|Starbucks| at-loc.p ($ loc 0 2 0)) (|Twitter| at-loc.p ($ loc 0 4 0)) (|Texaco| at-loc.p ($ loc 3 0 0)) (|McDonalds| at-loc.p ($ loc 4 0 0)) (|Mercedes| at-loc.p ($ loc 5 0 0)) (|Toyota| at-loc.p ($ loc 6 0 0)) (|Burger King| at-loc.p ($ loc 7 0 0))      (|Twitter| ((past move.v) (from.p-arg ($ loc 2 0 0)) (to.p-arg ($ loc 0 4 0)))) )
; ((|Target| at-loc.p ($ loc 0 0 0)) (|Starbucks| at-loc.p ($ loc 0 2 0)) (|Twitter| at-loc.p ($ loc 0 4 0)) (|Texaco| at-loc.p ($ loc 3 0 0)) (|McDonalds| at-loc.p ($ loc 4 0 0)) (|Mercedes| at-loc.p ($ loc 5 0 0)) (|Toyota| at-loc.p ($ loc 6 0 0)) (|Burger King| at-loc.p ($ loc 7 0 0))  )
;


(defun recall-answer (object-locations ulf)
; ````````````````````````````````````````````
; Given current observed block locations and the ULF of the query, recall the answer by consulting
; historical record of block moves stored in context.
;
  (format t "object locations: ~a~%" object-locations) ; DEBUGGING
  (let* ((ulf-base (uninvert-question (remove-not (remove-adv-f (remove-adv-e ulf)))))
         (coords (extract-coords object-locations)) (quant-times (get-referred-times coords ulf ulf-base))
         (quantifier (first quant-times)) (times (second quant-times))
         when-question where-question neg ans relation subj obj)
    (format t "base ulf: ~a~%" ulf-base)
    (format t "blocks at coordinates: ~a~%" coords)
    (format t "quantifier + referred times: ~a ~a~%" quantifier times) ; DEBUGGING

    ; Flag question if negation is present
    ; NOTE: we currently assume no possibility of double negation
    (when (ttt:match-expr '(^* not) ulf)
      (setq neg t))

    ; Detect if question is a where-question, if so we want to find all relations that held at a time. Also,
    ; it seems to only make sense to use the most-recent quantifier with where-questions.
    (when (ttt:match-expr '(^* (at.p (what.d place.n))) ulf)
      (setq where-question t)
      (setq quantifier 'most-recent))

    ; Detect if question is a when-question, if so we want to return times as the answer rather than
    ; spatial relation propositions.
    (when (and (ttt:match-expr '(^* (hist-prep-during? wh-np?)) ulf) (not where-question))
      (setq when-question t))

    ; Extract subject (needed for where-questions) and/or object (needed for "what block did I move" questions)
    (setq subj (extract-subj ulf-base))
    (setq obj (extract-obj ulf-base))
    (format t "subj: ~a~%" subj)
    (format t "obj: ~a~%" obj) ; DEBUGGING

    ; Extract relation from question, possibly including variables & restrictors for the subject and/or object.
    (setq relation (extract-relation ulf-base))
    (format t "extracted relation: ~a~%" relation) ; DEBUGGING

    ; Detect different types of historical questions which might be asked. Some are straightforward spatial questions
    ; but just asked about the past, whereas others have to do with the actions themselves, e.g. "what blocks did I move?"
    ; TODO: this is currently all very simplistic (for both scenarios), and needs to be improved in the future.
    (cond
      ; If asking a where-question
      (where-question
        (apply-to-times `(compute-relations ,subj ,coords) times quantifier when-question))
      ; If asking about spatial relation
      (relation
        (apply-to-times `(compute-relation ,relation ,coords ,neg) times quantifier when-question))
      ; Otherwise assume the question is about block moves
      (t
        (apply-to-times `(compute-move ,obj) times quantifier when-question)))
  )
) ; END recall-answer


(defun get-referred-times (coords ulf ulf-base)
; ```````````````````````````````````````````````
; Given a historical question ULF, get the list of times referred to by the ULF (for instance,
; by the adv-e phrases/lexical words), and a quantifier over those times (by default, most-recent)
  (let (adv-e-phrase adv-e-word quantifier times-phrase times-word times)
    ; Retrieve any adv-e phrase and adv-e lexical word in ulf
    (setq adv-e-phrase (extract-adv-e-phrase ulf))
    (setq adv-e-word (extract-adv-e-word ulf))
    ; Resolve adv-e phrase and adv-e word to times (or quantifier over times in
    ; the case of the latter)
    ;; (format t "adv-e-phrase: ~a~%" adv-e-phrase)
    ;; (format t "adv-e-word: ~a~%" adv-e-word) ; DEBUGGING
    (when adv-e-phrase
      (setq times-phrase (get-times-from-adv-e-phrase coords adv-e-phrase)))
    (when adv-e-word
      (setq times-word (get-times-from-adv-e-word coords adv-e-word)))
    ;; (format t "times-phrase: ~a~%" times-phrase)
    ;; (format t "times-word: ~a~%" times-word) ; DEBUGGING

    (cond
      ; If lexical adv-e gives some special quantifier
      ((and times-word (not (listp times-word)))
        (setq quantifier times-word)
        (setq times times-phrase))
      ; If both phrasal adv-e and lexical adv-e give list of times
      ((and times-word times-phrase)
        (setq times (intersection times-word times-phrase)))
      ; If only lexical adv-e gives list of times
      (times-word (setq times times-word))
      ; If only phrasal adv-e gives list of times
      (times-phrase (setq times times-phrase)))

    ; For simple questions about actions (e.g. "what block(s) did I move?"), the quantifier also seems to
    ; depend on the plurality of the subject, i.e. "what blocks did I move" looks over all times (if no
    ; adv-e is specified, the times between the previous utterance and the current utterance), whereas
    ; "what block did I move" looks only at the most recent time (since times correspond to individual moves).
    (cond
      ; Plural or "how many"
      ((or (ttt:match-expr '(_! ((tense? action-verb?) (! wh-pron? (det? (^* (plur noun?)))) _*)) ulf-base)
           (ttt:match-expr '(_! ((tense? aspect?) (action-verb? (! wh-pron? (det? (^* (plur noun?)))) _*))) ulf-base)
           (ttt:match-expr '(_! ((tense? action-verb?) ((nquan (how.mod-a many.a)) (plur noun?)) _*)) ulf-base))
        (if (not times) (setq times (get-times-before *time* (diff-times *time* *time-prev*)))
          (setq times (time-inclusive times)))
        (setq quantifier 'ever))
      ; Singular
      ((ttt:match-expr '(_! ((tense? action-verb?) (det? (^* noun?)) _*)) ulf-base)
       (ttt:match-expr '(_! ((tense? aspect?) (action-verb? (det? (^* noun?)) _*))) ulf-base)
        (if (not times) (setq times (get-times-before *time* 1))
          (setq times (time-inclusive times)))
        (setq quantifier 'most-recent)))
    ; By default, look at all times before now, and quantifier is just the most recent.
    (if (not times) (setq times (get-times-before *time* -1)))
    (if (not times) (setq times (list *time*)))
    (if (not quantifier) (setq quantifier 'most-recent))

  (list quantifier times))
) ; END get-referred-times


(defun extract-adv-e-phrase (ulf)
; `````````````````````````````````
; Extracts an adv-e phrase from a ULF, and applies any sub macros in the phrase.
;
  (let ((adv-e-phrase
      (ttt:apply-rule '(/ (^* (adv-e _!)) (adv-e _!))
        (apply-sub-macro ulf) :shallow t)))
    (if (adv-e? adv-e-phrase) adv-e-phrase nil))
) ; END extract-adv-e-phrase


(defun extract-adv-e-word (ulf)
; `````````````````````````````````
; Extracts an adv-e word from a ULF.
;
  (let ((adv-e-word (ttt:apply-rule '(/ (^* adv-e-lex?) adv-e-lex?) ulf :shallow t)))
    (if (adv-e? adv-e-word) adv-e-word nil))
) ; END extract-adv-e-phrase


(defun extract-relation (ulf)
; `````````````````````````````
; Extracts a relation (e.g. (|Twitter| on.p |Texaco)) from a ULF.
; If subject or object is not definite, use variables with restrictors, e.g.
; (?x on.p ?y), ((?x red.a) on.p |Texaco|), etc.
; TODO: what if ulf contains a negated relation, e.g. "is not on the Twitter block"
;
  (let ((relation
      (ttt:apply-rules '(
        ; Straightforward predicative cases
        (/ (_!1 ((tense? be.v) (^* (between.p (set-of _!2 _!3)))))
           ((resolve-rel-np! _!1) between.p (resolve-rel-np! _!2) (resolve-rel-np! _!3)))
        (/ (_!1 ((tense? be.v) (^* (prep? _!2))))
           ((resolve-rel-np! _!1) prep? (resolve-rel-np! _!2)))
        (/ (_!1 ((tense? aspect?) (be.v (^* (between.p (set-of _!2 _!3))))))
           ((resolve-rel-np! _!1) between.p (resolve-rel-np! _!2) (resolve-rel-np! _!3)))
        (/ (_!1 ((tense? aspect?) (be.v (^* (prep? _!2)))))
           ((resolve-rel-np! _!1) prep? (resolve-rel-np! _!2)))
        ; "what block did I put on the Twitter block?" (TODO: currently this is just treated as
        ; meaning the same as "what block was on the Twitter block", but it really is asking about
        ; this PLUS some block that I actually moved recently).
        (/ (_! ((tense? verb-untensed?) _!1 (between.p (set-of _!2 _!3))))
           ((resolve-rel-np! _!1) between.p (resolve-rel-np! _!2) (resolve-rel-np! _!3)))
        (/ (_! ((tense? verb-untensed?) _!1 (prep? _!2)))
           ((resolve-rel-np! _!1) prep? (resolve-rel-np! _!2)))
        ; "what block touches the Twitter block?" (TODO: needs to be generalized)
        (/ (_!1 ((tense? spatial-verb?) _!2))
           ((resolve-rel-np! _!1) (spatial-verb-to-prep! spatial-verb?) (resolve-rel-np! _!2)))
        (/ (_!1 ((tense? aspect?) (spatial-verb? _!2)))
           ((resolve-rel-np! _!1) (spatial-verb-to-prep! spatial-verb?) (resolve-rel-np! _!2)))
      )
      ulf)))
    (if (relation-prop? relation) relation nil))
) ; END extract-relation


(defun extract-subj (ulf)
; ````````````````````````
; Extracts a subject (e.g. |Twitter|) of a ULF.
; If subject is not definite, use variables with restrictors, e.g.
; ?x or (?x red.a).
  (let ((subj
      (ttt:apply-rules '(
        (/ (_!1 (! (^* (tense? verb-untensed?)))) (resolve-rel-np! _!1)))
      ulf))) 
    (if (or (nnp? subj) (variable? subj) (restricted-variable? subj)) subj nil))
) ; END extract-subj


(defun extract-obj (ulf)
; ````````````````````````
; Extracts an object (e.g. |Twitter|) of a ULF.
; If object is not definite, use variables with restrictors, e.g.
; ?x or (?x red.a).
  (let ((obj
    (ttt:apply-rules '(
      (/ (_! (^* ((tense? verb-untensed?) (^*2 (! wh-pron? (det? _!1)))))) (resolve-rel-np! !))
      (/ (_! (^* ((tense? aspect?) (verb-untensed? (^*2 (! wh-pron? (det? _!1))))))) (resolve-rel-np! !))
    )
    ulf)))
  (if (or (nnp? obj) (variable? obj) (restricted-variable? obj)) obj nil))
) ; END extract-obj


(defun resolve-rel-np! (ulf)
; ```````````````````````````
; Turns a np ULF into a corresponding proper name if something like (the.d (|Twitter| block.n)),
; otherwise turns it into a variable (or a variable with a list of adj restrictors).
; TODO: deal with more complex restrictors, such as n+preds, superlatives, multiple adjectives,
; conjunctions such as ((set-of (the.d (|Twitter| block.n)) (the.d (|Starbucks| block.n)))) ...
;
  (ttt:apply-rules '(
    (/ (det? (nnp? noun?)) nnp?)
    (/ (det? (color? noun?)) ((new-var!) (color-of.f color?)))
    (/ (det? _!) (new-var!))
    (/ wh-pron? (new-var!))
  ) ulf)
) ; END resolve-rel-np!


(defun extract-coords (prop-list)
; `````````````````````````````````
; Extracts all coordinates from a list of propositions, returned in the form: (|Name| ?x ?y ?z)
; NOTE: assuming that all at-loc.p propositions use explicit location record, i.e. ($ loc ?x ?y ?z)
;
  (mapcar (lambda (prop) (append (list (first prop)) (cddr (third prop))))
    (remove-if-not #'at-loc-prop? prop-list))
) ; END extract-coords


(defun extract-moves (prop-list)
; ```````````````````````````````
; Extracts all moves from a list of propositions, returned in the form:
; ((|Name| ?x1 ?y1 ?z1) (|Name| ?x2 ?y2 ?z2))
; Although the |Name| is duplicated here, it makes it easy to process later on.
; NOTE: assuming that all move.v propositions use explicit location record, i.e. ($ loc ?x1 ?y1 ?z1)
  (mapcar (lambda (prop)
      (list (append (list (first prop)) (cddr (second (second (second prop)))))
            (append (list (first prop)) (cddr (second (third (second prop)))))))
    (remove-if-not #'move-prop? prop-list))
) ; END extract-moves


(defun get-times-from-adv-e-word (coords ulf)
; ````````````````````````````````````````````
; Resolves a lexical adv-e into either a list of times (for example,
; something like "previously"), or an operator over times (e.g. "ever").
;
  (let ((time-funcall (ttt:apply-rules
      `((/ hist-adv-prev? (get-times-before ,*time* -1))
        (/ hist-adv-next? (get-times-after ,*time* -1))
        (/ hist-adv-init? (get-times-init 1))
        (/ hist-adv-cur? (list ,*time*))
        (/ hist-adv-recent? (identity most-recent))
        (/ hist-adv-just? (identity most-recent))
        (/ hist-adv-always? (identity always))
        (/ hist-adv-ever? (identity ever))
        (/ hist-adv-never? (identity not-ever))
        (/ (not-fbound? _*) (identity nil)))
      ulf)))
    (apply (car time-funcall) (cdr time-funcall)))
) ; END get-times-from-adv-e-word


(defun get-times-from-adv-e-phrase (coords ulf)
; ```````````````````````````````````````````````
; Resolves a phrasal adv-e (e.g. "before I moved it") into a list of times.
;
  (let ((adv-e (cadr (extract-adv-e-phrase ulf))))
    (cond
      ((prep-conjunction? adv-e)
        (intersection (get-times-from-pp coords (first adv-e)) (get-times-from-pp coords (third adv-e))))
      (t (get-times-from-pp coords adv-e))))
) ; END get-times-from-adv-e-phrase


(defun get-times-from-pp (coords ulf)
; `````````````````````````````````````
; Gets a list of times corresponding to a prepositional phrase,
; given the embedded noun phrase or reified event.
; NOTE: this function could use some cleaning.
;
  (let* ((adv-a (if (hist-adv-directly? (first ulf)) (first ulf)))
         (prep (progn (when adv-a (setq ulf (second ulf))) (first ulf)))
         ; Get reference times denoted by the reified event/noun phrase, which are
         ; to be modified by the preposition.
         (ref-times (cond
          ((reified-event? (second ulf))
            (get-times-from-ke coords (second ulf)))
          ((indexical-np? (second ulf))
            (list *time*))
          ((quant-np? (second ulf))
            (last (get-times-before *time*
              (numerical-det! (first (second ulf))))))
          ((definite-np? (second ulf))
            (get-times-from-np coords (second ulf))))))
    ; Apply predicate operation (before, after, during), along with any modifying
    ; adverb (e.g. "right before").
    (cond
      ; "just before"
      ((and adv-a (hist-prep-prev? prep)) (set-difference (union1 (mapcar
        (lambda (ref-time) (get-times-before ref-time 1)) ref-times)) ref-times))
      ; "before"
      ((hist-prep-prev? prep)
        (set-difference (get-times-before (car ref-times) -1) ref-times))
      ; "just after"
      ((and adv-a (hist-prep-next? prep)) (set-difference (union1 (mapcar
        (lambda (ref-time) (get-times-after ref-time 1)) ref-times)) ref-times))
      ; "after"
      ((hist-prep-next? prep)
        (set-difference (get-times-after (car ref-times) -1) ref-times))
      (t ref-times)))
) ; END get-times-from-pp


(defun get-times-from-ke (coords ulf)
; `````````````````````````````````````
; Gets a list of times corresponding to a reified event.
;
  (let ((ke-funcall (ttt:apply-rules
      `((/ (^* ((past move.v) (det? (nnp? noun?)))) (get-time-of-move nnp?))
        (/ (ke ((det? (nnp?1 noun?)) ((past be.v) (prep? (det? (nnp?2 noun?))))))
           (get-time-of-relation ,coords (nnp?1 prep? nnp?2)))
        (/ (ke _*) (identity nil)))
      ulf)) ke-result)
    (setq ke-result (apply (car ke-funcall) (cdr ke-funcall)))
    (if ke-result (list ke-result)))
) ; END get-times-from-ke


(defun get-times-from-np (coords ulf)
; `````````````````````````````````````
; Gets a list of times corresponding to a definite noun phrase.
; TODO: This will need updating once support for n+preds (e.g. "the
; turn where I moved the SRI block") is added.
;
  (let ((time-funcall (ttt:apply-rules
      `((/ (det? (adj? hist-noun-turn?)) (hist-adj! adj? 1))
        (/ (det? (adj? (? numerical-adj?) (plur hist-noun-turn?))) (hist-adj! adj? ?))
        (/ (det? (adj? hist-noun-prev?)) (get-times-before ,*time* 3))
        (/ (det? (adj? hist-noun-next?)) (get-times-after ,*time* 3))
        (/ (det? hist-noun-prev?) (get-times-before ,*time* -1))
        (/ (det? hist-noun-next?) (get-times-after ,*time* -1))
        (/ (det? hist-noun-init?) (get-times-init 1))
        (/ (not-fbound? _*) (identity nil)))
      ulf)))
    (apply (car time-funcall) (cdr time-funcall)))
) ; END get-times-from-np


(defun hist-adj! (adj &optional num)
; ````````````````````````````````````
; Maps a temporal adjective + n ("window size" of time period) to a
; function application to retrieve corresponding (discrete) times.
;
  (let ((n (cond ((null num) 3) ((numberp num) num) (t (numerical-adj! num)))))
    (ttt:apply-rules
    `((/ hist-adj-prev? (get-times-before ,*time* ,n))
      (/ hist-adj-next? (get-times-after ,*time* ,n))
      (/ hist-adj-init? (get-times-init ,n))
      (/ hist-adj-final? (get-times-final ,n))
      (/ hist-adj-cur? (list ,*time*))
      (/ numerical-adj? (get-time-nth ,(numerical-adj! adj))))
    adj))
) ; END hist-adj!


(defun reconstruct-scene (coords Tn)
; `````````````````````````````````````
; Reconstruct the scene (i.e. a list of coordinates for each block)
; at the time denoted by Tn, given current coordinates.
; coords should be a list of coordinates in the simplified form (|Name| ?x ?y ?z)
;
  (let* ((Ti (get-prev-time *time*)) (scene coords) moves)
    (loop while (and Ti (> (compare-time Ti Tn) -1) (> (compare-time Ti 'NOW0) -1)) do
      (setq moves (extract-moves (gethash Ti *context*)))
      (mapcar (lambda (move)
        (setq scene (subst (first move) (second move) scene :test #'equal))) moves)
      (setq Ti (get-prev-time Ti)))
    scene)
) ; END reconstruct-scene


(defun form-pred-list (coords-list1 prep-list coords-list2)
; ```````````````````````````````````````````````````````````
; Form predicates from all relations that are satisfied having
; things from coords-list1 as the subject, a preposition from
; prep-list, and coords-list2 as the object.
; NOTE: preds are returned in decreasing order of certainty.
; TODO: if between.p is added to the spatial-prep-list, this function will need adjusting.
;
  (let ((pred-list (remove nil
        ; Check all combination of blocks between coords-list1 and coords-list2
        (mapcan (lambda (coords1) (mapcan (lambda (coords2)
          (if (not (equal (car coords1) (car coords2)))
            ; Evaluate all prepositions in prep-list
            (mapcar (lambda (prep)
              (let ((certainty (eval-relation prep coords1 coords2)))
                ; If certainty is greater than zero, add tuple + certainty to pred-list
                (if (and (numberp certainty) (> certainty 0))
                  (list (list (car coords1) prep (car coords2)) certainty))))
            prep-list)))
          coords-list2)) coords-list1))))
    ; Sort by certainty
    (mapcar #'first (sort (copy-seq pred-list) #'> :key #'second)))
) ; END form-pred-list


(defun compute-relations (Ti subj coords)
; `````````````````````````````````````````
; Computes all spatial relations that hold at a particular time, for a particular
; subject (may be a variable with or without restrictors).
; NOTE: we assume uniqueness of coords in the scene, or else this will break.
; TODO: if between.p is added to the spatial-prep-list, this function will need adjusting.
;
  (let* ((scene (reconstruct-scene coords Ti))
        ; Find all possible pairs of subject + object in the scene, and check if relation holds
        (relations (form-pred-list scene *spatial-prep-list* scene)))
    ; Filter relations
    (last (reverse (find-cars-var subj relations)) 2))
) ; END compute-relations


(defun compute-relation (Ti rel coords neg)
; ```````````````````````````````````````````
; Computes a relation at a particular time (relation may include variables with/without
; restrictors, in which case it returns a list of all relations satisfying that form).
; TODO: if between.p is added to the spatial-prep-list, this function will need adjusting.
; TODO: figure out what to do with negation.
;
  (let* ((subj (first rel)) (prep (second rel)) (obj (third rel)) (obj2 (fourth rel))
        (scene (reconstruct-scene coords Ti))
        (coords-list1 (find-cars-var subj scene)) (coords-list2 (find-cars-var obj scene)))
    (form-pred-list coords-list1 (list prep) coords-list2))
) ; END compute-relation


(defun compute-move (Ti obj)
; `````````````````````````````
; Computes all moves at a particular time with the given object (may be a variable with/without
; restrictors).
; NOTE: this is a bit clunky code-style-wise.
;
  (let* ((moves (mapcar (lambda (move)
          (list (caar move) (cdar move) (cdadr move))) (extract-moves (gethash Ti *context*)))))
    (mapcar (lambda (move) `(,(car move) (past move.v))) (find-cars-var obj moves)))
) ; END compute-move


(defun apply-to-times (f times quantifier when-question)
; `````````````````````````````````````````````````````````
; Given a list f consisting of a function call plus arguments which returns a list of relations,
; apply it to a list of times based on the given quantifier, and combine the resulting relations
; in some way based on the quantifier (e.g. 'ever' is union whereas 'always' is intersection).
; If when-question is given, return times rather than relations. If neg is given, use negated
; relation.
; NOTE: currently certainties are added in at this step, and are just set equal to 1. It seems like
; there are two options here for the future: either certainties might reflect the "distance" of the
; time from the present, i.e. answers from earlier times are more uncertain, or the uncertainties might
; reflect something from the calculation of the relations themselves.
; TODO: "not-ever" is going to be a bigger challenge than I anticipated - what relations is it even supposed
; to return if e.g. the Twitter block was never on the Starbucks block?
; Actually, might not be that tricky. I need to ask Georgiy how he currently handles "what block is not on the
; Twitter block" - "not-ever ..." is essentially equivalent to "always not ..."
;
  ; If quantifier is most-recent, simply replace times with only the most recent time in the list
  (when (equal quantifier 'most-recent)
    (setq times (most-recent times)))
  ; Apply function to each time, and create a list of the time paired with all returned relation
  (let* ((time-rels (remove-if (lambda (x) (null (second x))) (mapcar (lambda (time)
          (list time (apply (car f) (cons time (cdr f))))) times)))
         (answers (mapcar (lambda (time-rel)
          (if when-question
            (list (add-certainty (first time-rel) (first time-rel)))
            (add-certainty-list (second time-rel) (first time-rel)))) time-rels)))
    ; Combine answers depending on the quantifier given
    (cond
      ((equal quantifier 'most-recent)
        (car answers))
      ((equal quantifier 'ever)
        (union1 answers))
      ((equal quantifier 'always)
        (intersection1 answers))
      ((equal quantifier 'not-ever)
        nil)))
) ; END apply-to-times


(defun add-certainty (rel time)
; ```````````````````````````````
; Adds certainty to a relation based on time.
; NOTE: currently always set to 1.0 - see note on 'apply-to-times' func.
;
  (list rel 1.0)
) ; END add-certainty


(defun add-certainty-list (rels time)
; ````````````````````````````````````
; Adds certainties to a list of relations.
; NOTE: currently always set to 1.0 - see note on 'apply-to-times' func.
;
  (mapcar (lambda (rel) (add-certainty rel time)) rels)
) ; END add-certainty-list


(defun get-time-of-relation (coords rel &key neg)
; `````````````````````````````````````````````````
; Get the most recent time at which a given relation held.
; If neg is given as t, get the most recent time at which the relation was not present.
;
  (labels ((get-time-of-relation-recur (rel Ti)
      (let* ((scene (reconstruct-scene coords Ti))
             (coords1 (find-car (first rel) scene)) (coords2 (find-car (third rel) scene))
             (rel-true (eval-relation-bool (second rel) coords1 coords2)))
        (cond
          ((and neg (not rel-true)) Ti)
          ((and (not neg) rel-true) Ti)
          ((equal Ti 'NOW0) nil)
          (t (get-time-of-relation-recur rel (get-prev-time Ti)))))))
    (get-time-of-relation-recur rel (get-prev-time *time*)))
) ; END get-time-of-relation


(defun get-time-of-move (name)
; ``````````````````````````````
; Gets the most recent time at which an object of a given name was moved.
;
  (labels ((get-time-of-move-recur (name Ti)
      (let* ((moves (extract-moves (gethash Ti *context*)))
             (moved-blocks (mapcar #'caar moves)))
        (cond
          ((null Ti) nil)
          ((member name moved-blocks) Ti)
          ((equal Ti 'NOW0) nil)
          (t (get-time-of-move-recur name (get-prev-time Ti)))))))
    (get-time-of-move-recur name (get-prev-time *time*)))
) ; END get-time-of-move










;; (defun get-moves-at-time (Ti)
;; ; ````````````````````````````
;; ; Gets the moves which happened during Ti
;; ;
;;   (mapcar (lambda (move) `((,(caar move) (past move.v)) 1)) (extract-moves (gethash Ti *context*)))
;; ) ; END get-moves-at-time


;; (defun get-moves-at-times (T-list)
;; ; ```````````````````````````````
;; ; Gets the moves which happened during the times in T-list.
;; ;
;;   (mapcan #'get-moves-at-time T-list)
;; ) ; END get-moves-at-times


;; (defun eval-relation-time (coords rel Ti &key neg)
;; ; ``````````````````````````````````````````````````
;; ; Determines whether a relation holds at a particular time.
;; ; If neg is given as t, return t if the relation doesn't hold at that time.
;; ;
;;   (let* ((scene (reconstruct-scene coords Ti))
;;          (coords1 (find-car (first rel) scene)) (coords2 (find-car (third rel) scene))
;;          (rel-true (eval-relation (second rel) coords1 coords2)))
;;     (if (or (and neg (not rel-true)) (and (not neg) rel-true)) t))
;; ) ; END eval-relation-time


;; (defun eval-relation-all-time (coords rel &key neg)
;; ; ```````````````````````````````````````````````````
;; ; Determines whether a relation holds over all times.
;; ; If neg is given as t, return t if *the relation doesn't hold at all times*.
;; ;
;;   (labels ((eval-relation-all-time-recur (rel Ti)
;;       (let ((rel-true (eval-relation-time coords rel Ti)))
;;         (cond
;;           ((equal Ti 'NOW0) rel-true)
;;           (t (and rel-true (eval-relation-all-time-recur rel (get-prev-time Ti))))))))
;;     (let ((result (eval-relation-all-time-recur rel (get-prev-time *time*))))
;;       (if (or (and neg (not result)) (and (not neg) result)) t)))
;; ) ; END eval-relation-all-time


;; (defun eval-relation-no-time (coords rel &key neg)
;; ; ```````````````````````````````````````````````````
;; ; Determines whether a relation never holds at any time.
;; ; If neg is given as t, return t if *the relation holds at some times*.
;; ;
;;   (labels ((eval-relation-no-time-recur (rel Ti)
;;       (let ((rel-true (eval-relation-time coords rel Ti :neg t)))
;;         (cond
;;           ((equal Ti 'NOW0) rel-true)
;;           (t (and rel-true (eval-relation-no-time-recur rel (get-prev-time Ti))))))))
;;     (let ((result (eval-relation-no-time-recur rel (get-prev-time *time*))))
;;       (if (or (and neg (not result)) (and (not neg) result)) t)))
;; ) ; END eval-relation-no-time


;; (defun count-moves (&key Ti block)
;; ; ``````````````````````````````````
;; ; Lists all moves.
;; ; If Ti is given, lists all moves since Ti.
;; ; If block is given, lists all moves with block as the subject.
;; ; 
;;   (if (null Ti) (setq Ti 'NOW0))
  
;; ) ; END count-moves









; TTT flags and other preds are defined as follows
; ``````````````````````````````````````````````````
(defun hist-adv-prev? (ulf)
  (member ulf '(previously.adv-e before.adv-e)))
(defun hist-adv-next? (ulf)
  (member ulf '(since.adv-e)))
(defun hist-adv-recent? (ulf)
  (member ulf '(recently.adv-e)))
(defun hist-adv-just? (ulf)
  (member ulf '(just.adv-e)))
(defun hist-adv-init? (ulf)
  (member ulf '(originally.adv-e initially.adv-e)))
(defun hist-adv-cur? (ulf)
  (member ulf '(currently.adv-e)))
(defun hist-adv-always? (ulf)
  (member ulf '(always.adv-e)))
(defun hist-adv-ever? (ulf)
  (member ulf '(ever.adv-e)))
(defun hist-adv-never? (ulf)
  (member ulf '(never.adv-e)))

(defun hist-noun-turn? (ulf)
  (member ulf '(turn.n stage.n step.n question.n iteration.n move.n period.n)))
(defun hist-noun-prev? (ulf)
  (member ulf '(past.n)))
(defun hist-noun-next? (ulf)
  (member ulf '(future.n)))
(defun hist-noun-init? (ulf)
  (member ulf '(start.n beginning.n)))

(defun hist-prep-during? (ulf)
  (member ulf '(at.p in.p on.p during.p ago.p)))
(defun hist-prep-prev? (ulf)
  (member ulf '(before.p prior_to.p preceding.p until.p)))
(defun hist-prep-next? (ulf)
  (member ulf '(after.p following.p since.p from.p)))

(defun hist-adj-prev? (ulf)
  (member ulf '(last.a previous.a preceding.a recent.a)))
(defun hist-adj-next? (ulf)
  (member ulf '(next.a following.a future.a)))
(defun hist-adj-init? (ulf)
  (member ulf '(first.a initial.a original.a)))
(defun hist-adj-final? (ulf)
  (member ulf '(final.a)))
(defun hist-adj-cur? (ulf)
  (member ulf '(current.a)))
(defun hist-adv-directly? (ulf)
  (member ulf '(directly.adv-a just.adv-a right.adv-a)))