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
  (let ((coords (extract-coords object-locations))
        (when-question (extract-when-question ulf)))
    ; If when question, remove when adv-e from ulf
    (setq ulf (remove-when-adv-e ulf))
    ; TODO: split off times (and add certainties) or return rels depending on whether when-question or not
    (find-answer-time+props coords ulf))
) ; END recall-answer


(defun find-answer-time+props (coords ulf)
; ``````````````````````````````````````````
; Given coordinates and a query ulf, return a list of lists of the form (time (prop1 prop2 ...)).
;
  (let* ((ulf-base (uninvert-question (remove-not (remove-adv-e (remove-adv-f ulf)))))
         (where-question (extract-where-question ulf))
         (neg (extract-neg ulf))
         (adv-e (extract-adv-e ulf))
         (adv-e-unary (resolve-adv-unary (first adv-e)))
         (adv-e-binary (resolve-adv-binary coords (second adv-e)))
         (adv-f (extract-adv-f ulf))
         (subj (extract-subj ulf-base))
         (obj (extract-obj ulf-base))
         (relation (extract-relation ulf-base))
         (action (extract-action ulf-base))
         func ans-time+props)
    (format t "where question: ~a~%" where-question)
    (format t "neg: ~a~%" neg)
    (format t "adv-e: ~a~%" adv-e)
    (format t "adv-e-unary: ~a~%" adv-e-unary)
    (format t "adv-e-binary: ~a~%" adv-e-binary)
    (format t "adv-f: ~a~%" adv-f)
    (format t "base ulf: ~a~%" ulf-base)
    (format t "subj: ~a~%" subj)
    (format t "obj: ~a~%" obj)
    (format t "extracted relation: ~a~%" relation)
    (format t "extracted action-verb: ~a~%" action)
    (format t "blocks at coordinates: ~a~%" coords)
    ;; (format t "quantifier + referred times: ~a ~a~%" quantifier times) ; DEBUGGING

    (cond
      (where-question
        (setq func `(compute-relations ,subj)))
      ((and relation action)
        (setq func `(compute-move+relation ,relation ,obj ,neg)))
      (relation
        (setq func `(compute-relation ,relation ,neg)))
      (action
        (setq func `(compute-move ,obj ,neg))))

    (setq ans-time+props (find+constrain-times func adv-e-unary adv-e-binary adv-f coords))
    (format t "ans-time+props: ~a~%" ans-time+props)
    
  ;; ans-time+props
  nil
  )
) ; END find-answer-time+props


(defun extract-where-question (ulf)
; ```````````````````````````````````
; Returns t if ULF contains an 'at what place' phrase, nil otherwise.
;
  (ttt:match-expr '(^* (at.p (what.d place.n))) ulf)
) ; END extract-where-question


(defun extract-when-question (ulf)
; ```````````````````````````````````
; Returns t if ULF contains a phrase like 'at what time' (and isn't a where question), nil otherwise.
;
  (and (ttt:match-expr '(^* (hist-prep-during? wh-np?)) ulf) (not (extract-where-question ulf)))
) ; END extract-when-question


(defun remove-when-adv-e (ulf)
; ``````````````````````````````
; Removes an adv-e such as "(at.p (what.d time.n))" from a ULF.
;
  (ttt:apply-rules '(
    (/ (sub (prep? wh-np?) _!) _!)
    (/ (_! (adv-e (! hole? (prep? wh-np?)))) _!)
    (/ (_*1 (adv-e (! hole? (prep? wh-np?))) _*2) (_*1 _*2))
  ) ulf)
) ; END remove-when-adv-e


(defun extract-neg (ulf)
; ````````````````````````
; Returns t if ULF is in a negative environment.
;
  (or (ttt:match-expr '(^* not) ulf)
      (ttt:match-expr '(^* never.adv-f) ulf))
) ; END extract-neg


(defun extract-action (ulf)
; ``````````````````````````
; Extracts a spatial action verb, e.g. put.v.
;
  (let ((action
      (ttt:apply-rules '(
        (/ (^* action-verb?) action-verb?))
      ulf :shallow t)))
    (if (action-verb? action) action nil))
) ; END extract-action


(defun extract-relation (ulf)
; `````````````````````````````
; Extracts a relation (e.g. (|Twitter| on.p |Texaco)) from a ULF.
; If subject or object is not definite, use variables with restrictors, e.g.
; (?x on.p ?y), ((?x red.a) on.p |Texaco|), etc.
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
        (/ (_!1 ((tense? (pasv verb-untensed?)) (between.p (set-of _!2 _!3))))
           ((resolve-rel-np! _!1) between.p (resolve-rel-np! _!2) (resolve-rel-np! _!3)))
        (/ (_!1 ((tense? (pasv verb-untensed?)) (prep? _!2)))
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
;
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
;
  (let ((obj
    (ttt:apply-rules '(
      (/ (_! (^* ((tense? verb-untensed?) (^*2 (! wh-pron? (det? _!1)))))) (resolve-rel-np! !))
      (/ (_! (^* ((tense? aspect?) (verb-untensed? (^*2 (! wh-pron? (det? _!1))))))) (resolve-rel-np! !)))
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


(defun extract-questions (prop-list)
; ```````````````````````````````````
; Extracts all questions from a list of propositions, returned in the form:
; (you 'ulf)
;
  (mapcar (lambda (prop)
      (list (first prop) (second (second prop))))
    (remove-if-not #'ask-prop? prop-list))
) ; END extract-questions


(defun extract-moves (prop-list)
; ```````````````````````````````
; Extracts all moves from a list of propositions, returned in the form:
; (|Name| (?x1 ?y1 ?z1) (?x2 ?y2 ?z2))
; NOTE: assuming that all move.v propositions use explicit location record, i.e. ($ loc ?x1 ?y1 ?z1)
;
  (mapcar (lambda (prop)
      (list (append (list (first prop)) (cddr (second (second (second prop)))))
            (append (list (first prop)) (cddr (second (third (second prop)))))))
    (remove-if-not #'move-prop? prop-list))
) ; END extract-moves


(defun extract-adv-e (ulf)
; ``````````````````````````
; Extracts all adv-e phrases from ULF (dividing into two sublists, one for unary preds,
; e.g. recent.a, and one for binary preds, e.g. before.p).
;
  (setq ulf (apply-sub-macro ulf))
  (let (adv-e-unary adv-e-binary)
    (labels ((extract-adv-e-recur (ulf-part)
        (cond
          ; If ULF is an adv-e with a unary pred, cons to adv-e-unary
          ((ttt:match-expr '(! (adv-e adj?) (adv-e (mod-a? adj?))) ulf-part)
            (setq adv-e-unary (cons ulf-part adv-e-unary)))
          ; If ULF is an adv-e with a binary pred, cons to adv-e-binary
          ((ttt:match-expr '(! (adv-e prep-phrase?) (adv-e (mod-a? prep-phrase?))) ulf-part)
            (setq adv-e-binary (cons ulf-part adv-e-binary)))
          ((ttt:match-expr '(! (sent-prep? _!) (mod-a? (sent-prep? _!))) ulf-part)
            (setq adv-e-binary (cons ulf-part adv-e-binary)))
          ; Otherwise, if ULF is atom, do nothing
          ((atom ulf-part) nil)
          ; Otherwise, if ULF is list, recur on each sub-part
          (t (mapcar #'extract-adv-e-recur ulf-part)))))
      (extract-adv-e-recur ulf))
    (list adv-e-unary adv-e-binary))
) ; END extract-adv-e


(defun extract-adv-f (ulf)
; ``````````````````````````
; Extracts all adv-f phrases from ULF.
;
  (setq ulf (apply-sub-macro ulf))
  (let (adv-f)
    (labels ((extract-adv-f-recur (ulf-part)
        (cond
          ; If ULF is an adv-f, cons to adv-f
          ((ttt:match-expr '(adv-f _!) ulf-part)
            (setq adv-f (cons ulf-part adv-f)))
          ; Otherwise, if ULF is atom, do nothing
          ((atom ulf-part) nil)
          ; Otherwise, if ULF is list, recur on each sub-part
          (t (mapcar #'extract-adv-f-recur ulf-part)))))
      (extract-adv-f-recur ulf))
    adv-f)
) ; END extract-adv-f


(defun resolve-adv-unary (adv-e-unary)
; ``````````````````````````````````````
; Resolves each adv-e in adv-e-unary, currently just to the unary adjective
; (possibly with a mod-a before it).
;
  (mapcar #'second adv-e-unary)
) ; END resolve-adv-unary


(defun resolve-adv-binary (coords adv-e-binary)
; ```````````````````````````````````````````````
; Resolves each adv-e in adv-e-binary to a simple binary predicate involving a
; time individual or set of time individuals.
; e.g. (adv-e (before.p (the.d (last.a turn.n)))) => (adv-e (before.p |Now3|))
;
  (mapcar (lambda (adv-e)
    (ttt:apply-rules `(
      (/ (prep? det-np?) (prep? (resolve-time-np! det-np?)))
      (/ (sent-prep? _!) ((ps-to-p! sent-prep?) (resolve-time-s! ,coords det-np?)))
    ) adv-e))
  adv-e-binary)
) ; END resolve-adv-binary


(defun resolve-time-np! (np)
; ```````````````````````````
; Resolves a temporal noun phrase to a corresponding time individual, or set
; of time individuals.
;
  'NOW0
) ; END resolve-time-np!


(defun resolve-time-s! (coords s)
; `````````````````````````````````
; Resolves a sentence to the time at which the sentence was true, or set of times.
; NOTE: in general this is going to be more tricky, because a sentential preposition
; clause may itself have adverbials, e.g. "where was the Twitter block before I moved
; it twice in a row", or "where was the Twitter block before I recently moved it". We
; do not consider these currently.
;
  'NOW0
  ;; (let (ps-result
  ;;     (ps-funcall (ttt:apply-rules `(
  ;;       ; before I put the Twitter block on the Texaco block
  ;;       (/ (^* ((past action-verb?) (det? (nnp? noun?)) (prep? (det? (nnp?2 noun?)))))
  ;;         (get-time-of-move+relation ,coords (nnp? prep? nnp?2)))
  ;;       ; before I moved the Twitter block
  ;;       (/ (^* ((past action-verb?) (det? (nnp? noun?))))
  ;;         (get-time-of-move nnp?))
  ;;       ; before the Twitter block was on the Texaco block
  ;;       (/ ((det? (nnp?1 noun?)) ((past be.v) (prep? (det? (nnp?2 noun?)))))
  ;;         (get-time-of-relation ,coords (nnp?1 prep? nnp?2)))
  ;;       (/ (not-fbound? _*) (identity nil)))
  ;;     s)))
  ;;   (setq ps-result (apply (car ps-funcall) (cdr ps-funcall)))
  ;;   (cond
  ;;     ((null ps-result) (list 'None))
  ;;     (t (make-set ps-result))))
) ; END resolve-time-s!


; ---------------------------------------





(defun form-pred-list (coords-list1 prep-list coords-list2 &key neg)
; ```````````````````````````````````````````````````````````````````
; Form predicates from all relations that are satisfied having
; things from coords-list1 as the subject, a preposition from
; prep-list, and coords-list2 as the object.
; If neg is given as t, return negated predicates.
; NOTE: preds are returned in decreasing order of certainty.
; TODO: if between.p is added to the spatial-prep-list, this function will need adjusting.
; TODO: this is kind of messy, could use some cleaning.
;
  (let ((pred-list (remove nil
        ; Check all combination of blocks between coords-list1 and coords-list2
        (mapcan (lambda (coords1) (mapcan (lambda (coords2)
          (if (not (equal (car coords1) (car coords2)))
            ; Evaluate all prepositions in prep-list
            (mapcar (lambda (prep)
              (let ((certainty (eval-spatial-relation prep coords1 coords2)))
                (if neg
                  ; If neg, add negated tuple + certainty for all with zero certainty
                  (if (and (numberp certainty) (<= certainty 0))
                    (list (list (car coords1) 'not prep (car coords2)) (- 1.0 certainty)))
                  ; Otherwise, if certainty is greater than threshold, add tuple + certainty to pred-list
                  (if (and (numberp certainty) (> certainty *certainty-threshold*))
                    (list (list (car coords1) prep (car coords2)) certainty))
                )))
            prep-list)))
          coords-list2)) coords-list1))))
    ; Sort by certainty
    (mapcar #'first (sort (copy-seq pred-list) #'> :key #'second)))
) ; END form-pred-list


(defun compute-relations (scene moves scene1 subj)
; ``````````````````````````````````````````````````
; Computes all spatial relations that hold at a particular scene, for a particular
; subject (may be a variable with or without restrictors).
; NOTE: we assume uniqueness of coords in the scene, or else this will break.
; TODO: if between.p is added to the spatial-prep-list, this function will need adjusting.
;
  ; Find all possible pairs of subject + object in the scene, and check if relation holds
  (let ((relations (form-pred-list scene *spatial-prep-list* scene)))
    ; Filter relations
    (last (reverse (find-cars-var subj relations)) 2))
) ; END compute-relations


(defun compute-move+relation (scene moves scene1 rel obj neg)
; `````````````````````````````````````````````````````````````
; Computes all moves of a block into a particular relation, i.e. whether the
; given object was moved, and that the relation holds in the resulting scene.
; If neg is given, either the object was not moved, or the relation didn't hold in the scene.
;
  (let ((relations (compute-relation scene1 rel nil)))
    ; Get rid of any moves for which the relation does not hold in scene1
    (setq moves (remove-if-not (lambda (move) (find move relations
                 :test (lambda (x y) (equal (car x) (car y))))) moves))
    ; If negation, find complement of moves
    (if neg (setq moves (negate-moves moves)))
    ; Simplify form of relations and filter based on obj
    (filter+process-moves moves obj))
) ; END compute-move+relation


(defun compute-relation (scene moves scene1 rel neg)
; ````````````````````````````````````````````````````
; Computes a relation at a particular scene (relation may include variables with/without
; restrictors, in which case it returns a list of all relations satisfying that form).
; TODO: if between.p is added to the spatial-prep-list, this function will need adjusting.
;
  (let* ((subj (first rel)) (prep (second rel)) (obj (third rel)) (obj2 (fourth rel))
        (coords-list1 (find-cars-var subj scene)) (coords-list2 (find-cars-var obj scene)))
    (form-pred-list coords-list1 (list prep) coords-list2 :neg neg))
) ; END compute-relation


(defun compute-move (scene moves scene1 obj neg)
; ````````````````````````````````````````````````
; Computes all moves at a particular time with the given object (may be a variable with/without
; restrictors).
;
  ; If negation, we need to access context to see all blocks (or more generally, 'movable entities'),
  ; and remove all of the blocks which moved during Ti.
  (if neg (setq moves (negate-moves moves)))
  ; Simplify form of relations and filter based on obj
  (filter+process-moves moves obj)
) ; END compute-move


(defun filter+process-moves (moves obj)
; ``````````````````````````````````````
; Filters all moves to ones involving the given object, and processes to form (|Texaco| (past move.v))
;
  (mapcar (lambda (move) `(,(car move) (past move.v))) (find-cars-var obj moves))
) ; END filter+process-moves


(defun negate-moves (moves)
; ``````````````````````````
; Given a list of block moves, return the complement of that list by accessing context.
;
  (set-difference (gethash 'block.n *context*) moves
    :test (lambda (x y) (equal (car x) (car y))))
) ; END negate-moves


(defun apply-binary-constraint (constraint time)
; ```````````````````````````````````````````````
; Applies a binary constraint (e.g. (BEFORE.P NOW2)) to time and returns t or nil.
;
  (let ((constraint-eval (ttt:apply-rule `(/ ((? mod-a?) (prep? _!))
          (eval-temporal-relation 'prep? ',time '_! '?)) constraint)))
    (eval constraint-eval))
) ; END apply-binary-constraint


(defun find+constrain-times (func adv-e-unary adv-e-binary adv-f coords)
; ```````````````````````````````````````````````````````````````````````
; Finds all times, constrained by the given adverbials, at which func holds. Return all
; satisfying times conjoined with the relevant propositions given by func.
; 
  (let ((f (car func)) (args (cdr func)) (Ti *time*) (scene coords) moves scene1 props times)
    ; Backtrack through times until the initial time is reached
    (loop while Ti do

      ; Get moves and 'backup' current scene (so we can use the scene of the consecutive turn)
      (setq moves (extract-moves (gethash Ti *context*)))
      (setq scene1 scene)

      ; Undo all moves that happened at current time to get new scene
      (mapcar (lambda (move)
        (setq scene (subst-move-scene move scene))) moves)

      ; Only consider this time if it satisfies all binary relations given by the binary adv-e phrases
      (when (every (lambda (adv-e) (apply-binary-constraint adv-e Ti) adv-e-binary))
      
        ; Call func using current scene, any moves, result scene, and any special args
        (setq props (apply f (append (list scene moves scene1) args)))
        ; If this gives a result, cons list of time and props to result list
        (when props
          (setq times (cons (list Ti props) times))))

      ; Go to previous time
      (setq Ti (get-prev-time Ti)))

    ; Apply all unary adv-e phrases to further constrain times
    'TODO

    ; Apply all adv-f phrases to select times which satisfy frequency
    'TODO

  times)
) ; END find+constrain-times


;; (defun reconstruct-scene (coords Tn)
;; ; `````````````````````````````````````
;; ; Reconstruct the scene (i.e. a list of coordinates for each block)
;; ; at the time denoted by Tn, given current coordinates.
;; ; coords should be a list of coordinates in the simplified form (|Name| ?x ?y ?z)
;; ;
;;   (let* ((Ti (get-prev-time *time*)) (scene coords) moves)
;;     (loop while (and Ti Tn (> (compare-time Ti Tn) -1) (> (compare-time Ti 'NOW0) -1)) do
;;       (setq moves (extract-moves (gethash Ti *context*)))
;;       (mapcar (lambda (move)
;;         (setq scene )) moves)
;;       (setq Ti (get-prev-time Ti)))
;;     scene)
;; ) ; END reconstruct-scene


;; (defun apply-to-times (f times quantifier when-question)
;; ; `````````````````````````````````````````````````````````
;; ; Given a list f consisting of a function call plus arguments which returns a list of relations,
;; ; apply it to a list of times based on the given quantifier, and combine the resulting relations
;; ; in some way based on the quantifier (e.g. 'ever' is union whereas 'always' is intersection).
;; ; If when-question is given, return times rather than relations. If neg is given, use negated
;; ; relation.
;; ; NOTE: currently certainties are added in at this step, and are just set equal to 1. It seems like
;; ; there are two options here for the future: either certainties might reflect the "distance" of the
;; ; time from the present, i.e. answers from earlier times are more uncertain, or the uncertainties might
;; ; reflect something from the calculation of the relations themselves.
;; ;
;;   ; Apply function to each time, and create a list of the time paired with all returned relation
;;   (let* ((time-rels (sort (copy-seq (remove-if (lambda (x) (null (second x)))
;;                 (mapcar (lambda (time) (list time (apply (car f) (cons time (cdr f))))) times)))
;;               (lambda (x y) (> (compare-time (car x) (car y)) 0))))
;;          (answers (mapcar (lambda (time-rel)
;;           (if when-question
;;             (list (add-certainty (first time-rel) (first time-rel)))
;;             (add-certainty-list (second time-rel) (first time-rel)))) time-rels)))
;;     ; Combine answers depending on the quantifier given
;;     (cond
;;       ((equal quantifier 'most-recent)
;;         (car answers))
;;       ((equal quantifier 'ever)
;;         (union1 answers))
;;       ((equal quantifier 'always)
;;         (intersection1 answers))))
;; ) ; END apply-to-times


(defun subst-move-scene (move scene)
; ````````````````````````````````````
; Substitutes the from.p coordinates into the to.p coordinates of a move in a given scene,
; effectively undoing the move.
;
  (subst (cons (first move) (second move)) (cons (first move) (third move)) scene :test #'~equal)
) ; END subst-move-scene


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








;; (defun get-time-of-relation (coords rel &key neg)
;; ; `````````````````````````````````````````````````
;; ; Get the time(s) at which a given relation held.
;; ; If neg is given as t, get the time(s) at which the relation did not hold.
;; ;
;;   (labels ((get-time-of-relation-recur (rel Ti)
;;       (let* ((scene (reconstruct-scene coords Ti))
;;              (coords1 (find-car (first rel) scene)) (coords2 (find-car (third rel) scene))
;;              (rel-true (eval-spatial-relation-bool (second rel) coords1 coords2)))
;;         (cond
;;           ((and neg (not rel-true)) Ti)
;;           ((and (not neg) rel-true) Ti)
;;           ((equal Ti 'NOW0) nil)
;;           (t (get-time-of-relation-recur rel (get-prev-time Ti)))))))
;;     (get-time-of-relation-recur rel (get-prev-time *time*)))
;; ) ; END get-time-of-relation


;; (defun get-time-of-move+relation (coords rel)
;; ; `````````````````````````````````````````````
;; ; Gets the most recent time at which an object of a given name was moved into a relation
;; ; with another object of a given name.
;; ;
;;   (labels ((get-time-of-move+relation-recur (rel Ti)
;;       (let* ((scene (reconstruct-scene coords (get-next-time Ti)))
;;              (name1 (first rel)) (prep (second rel)) (name2 (third rel))
;;              (coords1 (find-car name1 scene)) (coords2 (find-car name2 scene))
;;              (rel-true (eval-spatial-relation-bool prep coords1 coords2))
;;              (moves (extract-moves (gethash Ti *context*)))
;;              (moved-blocks (mapcar #'caar moves)))
;;         (cond
;;           ((null Ti) nil)
;;           ((and (member name1 moved-blocks) rel-true) Ti)
;;           ((equal Ti 'NOW0) nil)
;;           (t (get-time-of-move-recur name (get-prev-time Ti)))))))
;;     (get-time-of-move-recur name (get-prev-time *time*)))
;; ) ; END get-time-of-move+relation


;; (defun get-time-of-move (name)
;; ; ``````````````````````````````
;; ; Gets the most recent time at which an object of a given name was moved.
;; ;
;;   (labels ((get-time-of-move-recur (name Ti)
;;       (let* ((moves (extract-moves (gethash Ti *context*)))
;;              (moved-blocks (mapcar #'caar moves)))
;;         (cond
;;           ((null Ti) nil)
;;           ((member name moved-blocks) Ti)
;;           ((equal Ti 'NOW0) nil)
;;           (t (get-time-of-move-recur name (get-prev-time Ti)))))))
;;     (get-time-of-move-recur name (get-prev-time *time*)))
;; ) ; END get-time-of-move


;; (defun get-time-n-moves-before (Ti n)
;; ; ````````````````````````````````````
;; ; Gets the time of the nth most recent move.
;; ;
;;   (labels ((get-time-n-moves-before-recur (Tj j)
;;       (let* ((moves (extract-moves (gethash Tj *context*)))
;;              (k (- j (length moves))))
;;         (cond
;;           ((null Tj) nil)
;;           ((<= k 0) Tj)
;;           ((equal Tj 'NOW0) nil)
;;           (t (get-time-n-moves-before-recur (get-prev-time Tj) k))))))
;;     (get-time-n-moves-before-recur (get-prev-time Ti) n))
;; ) ; END get-time-n-moves-before


;; (defun get-time-n-moves-after (Ti n)
;; ; ````````````````````````````````````
;; ; Gets the time of the nth most recent move.
;; ;
;;   (labels ((get-time-n-moves-after-recur (Tj j)
;;       (let* ((moves (extract-moves (gethash Tj *context*)))
;;              (k (- j (length moves))))
;;         (cond
;;           ((null Tj) nil)
;;           ((<= k 0) Tj)
;;           ((equal Tj 'NOW0) nil)
;;           (t (get-time-n-moves-after-recur (get-next-time Tj) k))))))
;;     (get-time-n-moves-after-recur (get-next-time Ti) n))
;; ) ; END get-time-n-moves-after







; TTT flags and other preds are defined as follows
; ``````````````````````````````````````````````````
(defun hist-adv-prev? (ulf)
  (member ulf '(previously.adv-e before.adv-e last.adv-e)))
(defun hist-adv-next? (ulf)
  (member ulf '(since.adv-e)))
(defun hist-adv-recent? (ulf)
  (member ulf '(recently.adv-e)))
(defun hist-adv-just? (ulf)
  (member ulf '(just.adv-e)))
(defun hist-adv-init? (ulf)
  (member ulf '(originally.adv-e initially.adv-e first.adv-e)))
(defun hist-adv-cur? (ulf)
  (member ulf '(currently.adv-e now.adv-e)))
(defun hist-adv-ever? (ulf)
  (member ulf '(ever.adv-e)))

;; (defun hist-adv-once? (ulf)
;;   (member ulf '(once.adv-f)))
;; (defun hist-adv-twice? (ulf)
;;   (member ulf '(twice.adv-f)))
;; (defun hist-adv-always? (ulf)
;;   (member ulf '(always.adv-f)))
;; (defun hist-adv-never? (ulf)
;;   (member ulf '(never.adv-f)))

(defun hist-adv-directly? (ulf)
  (member ulf '(directly.adv-a just.adv-a right.adv-a)))

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

(defun hist-ps-prev? (ulf)
  (member ulf '(before.ps prior_to.ps preceding.ps until.ps)))
(defun hist-ps-next? (ulf)
  (member ulf '(after.ps following.ps since.ps from.ps)))
(defun hist-ps-while? (ulf)
  (member ulf '(while.ps when.ps)))

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