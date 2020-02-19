;; Feb 4/2020
;; ================================================
;;
;; This code is used to generate a response to a spatial query,
;; given a list of propositional answer relations with certainties.
;;
;; The general process is to categorize each query into one of a small
;; set of general question categories, such as "confirm" (yes/no question),
;; "ident" (what question), "descr" (where question), etc. This category is
;; used to form an appropriate answer ULF fragment (such as simple yes/no for
;; "confirm", a noun phrase for "ident", a set of prepositional phrases for "descr",
;; etc.). This answer ULF is then substituted in for the appropriate constituent in
;; the query ULF, such as what.pro/(what.d block.n) in the case of "ident".
;;
;; After this substitution, the modified query ULF is uninverted by applying all
;; sub operators and removing redundant auxiliaries to form the response ULF.
;; Certain pragmatic modifications are made at this point, chiefly checking for
;; and responding to presupposition failure using the ulf-pragmatics library.
;;

; Example queries:
; '(((THE.D (|Twitter| BLOCK.N)) ((PRES BE.V) (ON.P (THE.D (|SRI | BLOCK.N))))) ?)
; '(((WHAT.D BLOCK.N) ((PRES BE.V) (ON.P (THE.D (|SRI | BLOCK.N))))) ?)
; '(((WHAT.D (PLUR BLOCK.N)) ((PRES BE.V) (ON.P (THE.D (|SRI | BLOCK.N))))) ?)
; '(((WHAT.D (PLUR BLOCK.N)) ((PRES DO.AUX-S) NOT (TOUCH.V (THE.D (|SRI | BLOCK.N))))) ?)
; '(((WHAT.D (PLUR BLOCK.N)) ((PRES BE.V) NOT (TO_THE_LEFT_OF.P (THE.D (|Twitter| BLOCK.N))))) ?)
; '(((PRES BE.V) THERE.PRO (A.D BLOCK.N) (ON.P (THE.D (|SRI | BLOCK.N)))) ?)
; '(((PRES BE.V) THERE.PRO (K (PLUR BLOCK.N)) (ON.P (THE.D (|SRI | BLOCK.N)))) ?)
; '(((PRES DO.AUX-S) (SOME.D BLOCK.N) (TOUCH.V (THE.D (SRI  BLOCK.N)))) ?)
; '((SUB (OF.P (WHAT.D COLOR.N)) ((THE.D (MOST-N LEFT.A BLOCK.N)) ((PRES BE.V) *H))) ?)
; '((SUB (OF.P (WHAT.D COLOR.N)) ((THE.D (N+PREDS BLOCK.N (ON.P (THE.D (|Twitter| BLOCK.N))))) ((PRES BE.V) *H))) ?)
; '((((NQUAN (HOW.MOD-A MANY.A)) (RED.A (PLUR BLOCK.N))) ((PRES BE.V) (ON.P (THE.D TABLE.N)))) ?)
; '(((WHAT.D BLOCK.N) ((PRES BE.V) (BETWEEN.P ((THE.D (|SRI | BLOCK.N)) AND.CC (THE.D (|NVidia| BLOCK.N)))))) ?)
; '((SUB (AT.P (WHAT.D PLACE.N)) ((THE.D (|SRI | BLOCK.N)) ((PRES BE.V) *H))) ?)
; '((SUB (AT.P (WHAT.D PLACE.N)) ((THE.D (MOST-N LEFT.A (GREEN.A BLOCK.N))) ((PRES BE.V) *H))) ?)
; '((WHAT.PRO ((PRES BE.V) (= (THE.D (MOST-N LEFT.A BLOCK.N))))) ?)
; '((SUB (WHAT.D BLOCK.N) ((THE.D (|Twitter| BLOCK.N)) ((PRES BE.V) (ON_TOP_OF.P *H)))) ?)
; '(((WHAT.D (COLOR.A (PLUR BLOCK.N))) ((PRES BE.V) (TO_THE_LEFT_OF.P (THE.D (|Texaco| BLOCK.N))))) ?)
; '(((WHAT.D (COLOR.A (PLUR BLOCK.N))) ((PRES BE.V) (ON.P (THE.D TABLE.N)))) ?)
; '((SUB (OF.P (WHAT.D COLOR.N)) ((THE.D (|Texaco| BLOCK.N)) ((PRES BE.V) *H))) ?)
; '((SUB (WHAT.D (PLUR BLOCK.N)) ((PAST DO.AUX-S) I.PRO (MOVE.V *H))) ?)
; '(((THE.D (NVidia BLOCK.N)) (EVER.ADV-E ((PAST PERF) (TOUCH.V (THE.D (NVidia BLOCK.N)))))) ?)
; '((SUB (DURING.P (WHAT.D TURN.N)) ((PAST DO.AUX-S) I.PRO (MOVE.V (THE.D (|SRI | BLOCK.N)) (ADV-E *H)))) ?)
; '((SUB (WHAT.D BLOCK.N) ((THE.D (Twitter BLOCK.N)) ((PAST BE.V) INITIALLY.ADV-E (ON.P *H)))) ?)
; '(((THE.D (|Twitter| BLOCK.N)) ((PRES BE.V) (ON.P (WHICH.D BLOCK.N)))) ?)
; '((SUB (AT.P (WHAT.D PLACE.N)) ((THE.D (|Twitter| BLOCK.N)) ((PAST BE.V) *H (ADV-E (BEFORE.P (KE (I.PRO ((PAST MOVE.V) (THE.D (|Twitter| BLOCK.N)))))))))) ?)
; '((SUB (AT.P (WHAT.D PLACE.N)) ((THE.D (|Twitter|  BLOCK.N)) ((PAST BE.V) *H (ADV-E ((BEFORE.P (THE.D (SECOND.A TURN.N))) AND.CC (AFTER.P (THE.D (THIRD.A TURN.N)))))))) ?)
; '((SUB (WHAT.D (PLUR BLOCK.N)) ((PAST DO.AUX-S) I.PRO (MOVE.V *H (ADV-E (DURING.P (THE.D (INITIAL.A (PLUR TURN.N)))))))) ?)
; '((SUB (AT.P (WHAT.D PLACE.N)) ((PAST DO.AUX-S) I.PRO (MOVE.V (THE.D (Twitter BLOCK.N)) (ADV-A *H)))) ?)
; '((SUB (HOW_MANY.D (PLUR BLOCK.N)) ((PAST DO.AUX-S) I.PRO (MOVE.V *H))) ?)
; '((SUB (WHAT.D BLOCK.N) ((PAST DO.AUX-S) I.PRO (MOVE.V *H (ADV-E (SUB (TWO.D (PLUR TURN.N)) (AGO.P *H)))))) ?)
; '(((THE.D (|Target| BLOCK.N)) ((PAST BE.V) (EVER.ADV-E (ON.P (THE.D (|Starbucks| BLOCK.N)))))) ?)
; '(((THE.D (|Target| BLOCK.N)) ((PAST BE.V) (EVER.ADV-E (ON.P (THE.D (|Starbucks| BLOCK.N)))) (ADV-E (BEFORE.P (KE (I.PRO ((PAST MOVE.V) (THE.D (|Starbucks| BLOCK.N))))))))) ?)
; '((SUB (WHAT.D BLOCK.N) ((PAST DO.AUX-S) I.PRO (JUST.ADV-E (MOVE.V *H)))) ?)
; '((SUB (AT.P (WHAT.D PLACE.N)) ((THE.D (|Target| BLOCK.N)) ((PAST BE.V) *H (ADV-E (BEFORE.P (KE (IT.PRO ((PAST BE.V) (ON.P (THE.D (|Starbucks| BLOCK.N))))))))))) ?)
;
; Example relations:
; '()
; '(((|Starbucks| on.p |Target|) 0.6))
; '(((|Starbucks| on.p |Target|) 0.8))
; '(((|Starbucks| touch.v |Target|) 0.8))
; '(((|Starbucks| on.p |Target|) 0.8) ((|Twitter| on.p |Target|) 0.8))
; '(((|Starbucks| on.p |Target|) 0.8) ((|Twitter| on.p |Target|) 0.8) ((|Texaco| on.p |Target|) 0.8))
; '(((|Starbucks| on.p |Target|) 0.6) ((|Twitter| on.p |Target|) 0.8))
; '(((|Starbucks| on.p |Target|) 0.8) ((|Twitter| on.p |Target|) 0.6))
; '(((|Starbucks| on.p |Target|) 0.6) ((|Twitter| on.p |Target|) 0.6))
; '(((|Starbucks| on.p |Target|) 0.8) ((|Starbucks| near.p |Twitter|) 0.8))
; '(((|Starbucks| on.p |Target|) 0.8) ((|Starbucks| on.p |Twitter|) 0.8))
; '(((|Target| on.p |Starbucks|) 0.8) ((|Target| on.p |Texaco|) 0.8) ((|Target| near.p |Twitter|) 0.8))
; '(((|Target| on.p |Starbucks|) 0.8) ((|Target| near.p |Texaco|) 0.8) ((|Target| between.p |Mercedez| |Texaco|) 0.8) ((|Target| between.p |Twitter| |NVidia|) 0.8))
; '(((|Texaco| on.p |Twitter|) 0.8) ((|Starbucks| near.p |Twitter|) 0.8))
; '(((|Texaco| (past move.v)) 0.8) ((|Starbucks| (past move.v)) 0.8))
; '(((|Texaco| ((pres be.v) blue.a)) 0.8))
;


(defun generate-response (query-ulf relations)
; `````````````````````````````````````````````
; Generates a natural language response, given a query ULF and a list of satisfying relations+certainties.
; In the case where relations is nil, we rely on presupposition handling to generate the appropriate response.
;
  (let ((query-type (get-query-type query-ulf)) ans-tuple ans-ulf output-ulf uncertain-flag poss-ans)

    ; Check if poss-ques, if so set flag to true (to add a hedge later on) and set ulf to remainder
    (when (poss-ques? query-ulf)
      (setq poss-ans t)
      (setq query-ulf (second query-ulf)))

    ; ans-tuple consists of a list (ans-ulf uncertain-flag), where ans-ulf is a ULF corresponding
    ; to the relevant answer, and uncertain-flag indicates whether the answer is above or below the
    ; certainty threshold.
    (setq ans-tuple (form-ans query-type relations))
    (setq ans-ulf (first ans-tuple))
    (setq uncertain-flag (second ans-tuple))

    ;; (format t "Query: ~a~%" query-ulf)
    ;; (format t "Query type: ~a~%" query-type)
    ;; (format t "Relations: ~a~%" relations)
    ;; (format t "Answer: ~a~%" ans-ulf) ; DEBUGGING

    ; Make appropriate substitutions of answer ULF into query ULF,
    ; and uninvert form of question to get output ULF.
    (setq output-ulf (uninvert-question (cond
      ; Query is CONFIRM type
      ((equal query-type 'CONFIRM)
        ans-ulf)
      ; Query is ATTR-COLOR type
      ((equal query-type 'ATTR-COLOR)
        (ttt:apply-rule `(/ color-flag? ,ans-ulf) query-ulf))
      ; Query is COLOR-OBJECT type
      ((equal query-type 'COLOR-OBJECT)
        (ttt:apply-rule `(/ color-object-flag? ,ans-ulf) query-ulf))
      ; Query is COUNT type
      ((equal query-type 'COUNT)
        (ttt:apply-rules `((/ count-flag? ,ans-ulf)
          (/ ((nquan one.a) (_! (plur _!1))) ((nquan one.a) (_! _!1))) ; These two rules are to fix cases like
          (/ ((nquan one.a) (plur _!1)) ((nquan one.a) _!1))) query-ulf)) ; "one blocks" or "one red blocks"
      ; Query is EXIST type
      ((equal query-type 'EXIST)
        (if ans-ulf
          (ttt:apply-rule `(/ exist-flag? ,ans-ulf) query-ulf)
          '(NO.YN)))
      ; Query is IDENT-PREP type
      ((equal query-type 'IDENT-PREP)
        (ttt:apply-rule `(/ (prep? ident-flag?) (prep? ,ans-ulf)) query-ulf))
      ; Query is IDENT type
      ((equal query-type 'IDENT)
        (ttt:apply-rule `(/ ident-flag? ,ans-ulf) query-ulf))
      ; Query is DESCR type
      ((equal query-type 'DESCR)
        (ttt:apply-rule `(/ descr-flag? ,ans-ulf) query-ulf))
      ; Query is TIME type
      ((equal query-type 'TIME)
        (ttt:apply-rule `(/ time-flag? ,ans-ulf) query-ulf)))))

    ; When answer is uncertain, append an adverb indicating uncertainty to answer
    (when uncertain-flag
      (setq output-ulf (list 'possibly.adv-s output-ulf)))

    ; When no answer, check if a presupposition failure has occurred, if so, output the
    ; negation of that presupposition
    (if (null relations)
      (let ((presupposition-failure (respond-to-presupposition query-ulf)))
        ;; (format t "presupposition failure response: ~a~%" presupposition-failure) ; DEBUGGING
        (if presupposition-failure (setq output-ulf presupposition-failure))))

    ; Convert output ULF to an english string and output (or output an error if the output ULF is nil)
    (if output-ulf
      (append (if poss-ans '(You\'re not sure you understood the question correctly \, but))
        (ulf-to-english output-ulf))
      '(Sorry \, you was unable to find an object that satisfies given constraints \, please rephrase in a simpler way \.)))
) ; END generate-response


(defun get-query-type (ulf)
; ```````````````````````````
; Classify a ULF query as one of a few types by checking corresponding TTT flags.
; NOTE By default, we might want to treat queries as CONFIRM - the above flags have difficulties matching to
; queries which are given in declarative form, or parsed in such a way (for example, the parse of
; "has the Twitter block ever touched the Mercedez block?" removes the "has" and adds a perfect aspect
; to touch.v, so the output ULF resembles a question in declarative form).
;
  (cond
    ((ttt:match-expr '(^* color-flag?) ulf) 'ATTR-COLOR)
    ((ttt:match-expr '(^* color-object-flag?) ulf) 'COLOR-OBJECT)
    ((ttt:match-expr '(^* descr-flag?) ulf) 'DESCR)
    ((ttt:match-expr '(^* time-flag?)  ulf) 'TIME)
    ((ttt:match-expr '(^* ident-prep-flag?) ulf) 'IDENT-PREP)
    ((ttt:match-expr '(^* ident-flag?) ulf) 'IDENT)
    ((ttt:match-expr '(^* count-flag?) ulf) 'COUNT)
    ((ttt:match-expr '(^* exist-flag?) ulf) 'EXIST)
    (t 'CONFIRM))
) ; END get-query-type


(defun form-ans (query-type relations)
; `````````````````````````````````````````
; Get an answer (i.e. a ULF object reflecting the answer) depending on the broad type of query
; and the list of relations given.
;
  (let ((certain (certain-rels relations)) (uncertain (uncertain-rels relations))
        ans-set uncertain-flag ans)

    ; If any certain relations are known, discard the uncertain ones and construct the answer ULF from those.
    ; Otherwise, construct the answer ULF from the uncertain relations and add an uncertainty indicator at the end.
    (setq ans-set certain)
    (when (null ans-set)
      (setq ans-set uncertain)
      (if ans-set (setq uncertain-flag t)))

    ; Refine answer depending on what general category of question was asked.
    (setq ans (cond
      ; Query is CONFIRM type
      ((equal query-type 'CONFIRM)
        (if ans-set '(YES.YN) '(NO.YN)))
      ; Query is ATTR-COLOR type
      ((equal query-type 'ATTR-COLOR)
        (form-ans-color ans-set))
      ; Query is COLOR-OBJECT type
      ((equal query-type 'COLOR-OBJECT)
        (form-ans-color-object ans-set))
      ; Query is COUNT type
      ((equal query-type 'COUNT)
        (form-ans-count ans-set))
      ; Query is EXIST type
      ((equal query-type 'EXIST)
        (form-ans-subj ans-set))
      ; Query is IDENT-PREP type
      ((equal query-type 'IDENT-PREP)
        (form-ans-obj ans-set))
      ; Query is IDENT type
      ((equal query-type 'IDENT)
        (form-ans-subj ans-set))
      ; Query is DESCR type
      ((equal query-type 'DESCR)
        (form-ans-descr ans-set))
      ; Query is TIME type
      ((equal query-type 'TIME)
        (form-ans-time ans-set))
      ; Other
      (t
        '(Sorry \, you was unable to find an object that satisfies given constraints \, please rephrase in a simpler way \.))))

    (list ans uncertain-flag))
) ; END form-ans


(defun ulf-to-english (ulf)
; ``````````````````````````
; For converting a ULF response to a surface form response via the ulf2english library.
;
  ;; (format t "converting to english: ~a~%" ulf) ; DEBUGGING
  (str-to-output (ulf2english:ulf2english ulf :add-commas t))
) ; END ulf-to-english


(defun respond-to-presupposition (ulf)
; ``````````````````````````````````````
; Generates presupposition for query ULF (if any), and creates a response to that
; presupposition by negating it.
;
  (negate-wh-question-presupposition (normalize-wh-question-presupposition
    (ulf-pragmatics:get-wh-question-presupposition ulf :calling-package *package*)))
) ; END respond-to-presupposition


(defun normalize-wh-question-presupposition (ulf)
; `````````````````````````````````````````````````````````````
; Extracts result formula and applies task-specific normalization, including
; (for now) fixing bugs, such as the presupposition having "something.d block.n"
; instead of "some.d block.n".
;
  (ttt:apply-rules
    '((/ (something.d _!) (some.d _!))
      (/ (some.d (ulf:adj? (! ulf:noun? (plur ulf:noun?)))) (some.d !))
      (/ (nquan (somehow.mod-a many.a)) some.d)
      (/ (I.pro ((past do.aux-s) (ulf:verb? _! (adv-e (! (^* (some.d _!1)))))))
         (I.pro ((past ulf:verb?) _!)))
      (/ (I.pro ((past ulf:verb?) _! (adv-e (! (^* (some.d _!1))))))
         (I.pro ((past ulf:verb?) _!)))
      (/ ((a.d _!) ((tense? be.v) _*))
         ((some.d _!) ((tense? be.v) _*))))
    ulf)
) ; END normalize-wh-question-presupposition


(defun negate-wh-question-presupposition (ulf)
; `````````````````````````````````````````````
; Negates a presupposition by changing "some"/"something" to "no"/"nothing" (in
; the case of negations, we want to remove the double negative), or otherwise adding
; a negation to the ULF.
; NOTE: It seems like, in the future, this function could be made more robust using
; natural logic like inference rules, i.e. making use of NPI/PPI to get the
; "rewritings" correctly.
;
  (cond
    ((ttt:match-expr '(^* (! some.d something.pro)) ulf)
      (ttt:apply-rules
        '((/ something.pro nothing.pro)
          (/ (some.d _!) (no.d _!))
          (/ (nothing.pro ((? (tense? do.aux-s)) not (ulf:verb? _*))) (everything.pro ((tense? ulf:verb?) _*)))
          (/ ((no.d _!) ((? (tense? do.aux-s)) not (ulf:verb? _*))) ((every.d _!) ((tense? ulf:verb?) _*)))
          (/ (nothing.pro ((tense? ulf:verb?) not _*)) (everything.pro ((tense? ulf:verb?) _*)))
          (/ ((no.d _!) ((tense? ulf:verb?) not _*)) ((every.d _!) ((tense? ulf:verb?) _*)))
          (/ ((tense? do.aux-s) (^* (ulf:verb? (no.d _!)))) ((tense? do.aux-s) not (ulf:verb? (any.d _!))))
          (/ ((tense? do.aux-s) (^* (ulf:verb? nothing.pro))) ((tense? do.aux-s) not (ulf:verb? anything.d)))
          (/ (every.d (! (^* (plur ulf:noun?)))) (all.d !)))
      ulf))
    (t
      (ttt:apply-rules
        '((/ (_! ((tense? be.v) _*)) (_! ((tense? be.v) not _*)))
          (/ (_! ((tense? ulf:verb?) _*)) (_! ((tense? do.aux-s) not (ulf:verb? _*)))))
      ulf :max-n 1 :shallow t)))
) ; END negate-wh-question-presupposition


(defun form-ans-obj (relations)
; ``````````````````````````````````
; Retrieves objects from a list of relations
;
  (make-set (remove-duplicates (remove nil
    (mapcar (lambda (rel)
      (make-np (third rel) 'block.n)) relations)) :test #'equal))
) ; END form-ans-obj


(defun form-ans-subj (relations)
; ``````````````````````````````````
; Retrieves subjects from a list of relations
;
  (make-set (remove-duplicates (remove nil
    (mapcar (lambda (rel)
      (make-np (first rel) 'block.n)) relations)) :test #'equal))
) ; END form-ans-subj


(defun form-ans-count (set)
; ```````````````````````````
; Creates ULF determiner corresponding to the size of the answer set.
;
  (num-to-adj (length set))
) ; END form-ans-count


(defun form-ans-color (relations)
; ````````````````````````````````
; Retrieves colors from a list of relations.
;
  (make-set (remove-duplicates (remove nil
    (mapcar (lambda (rel)
      (get-color (first rel))) relations)) :test #'equal))
) ; END form-ans-color


(defun form-ans-color-object (relations)
; ````````````````````````````````````````
; Retrieves color noun phrases from a list of relations.
;
  (make-set (remove-duplicates (remove nil
    (mapcar (lambda (rel)
      (make-color-np (first rel) 'block.n)) relations)) :test #'equal))
) ; END form-ans-color


(defun form-ans-descr (relations)
; ```````````````````````````````````
; Creates a ULF conjunction of relations describing where the subject is.
;
  (when (equal relations 'None) (return-from conjoin-relations 'None))
  (cons 'set-of (mapcan (lambda (group)
    (if (and (listp group) (>= (length group) 2) (listp (second group)))
      (mapcan (lambda (rel) (list (list (second rel) (third rel)))) group)
      (list (list (second group) (third group)))))
    (mapcar #'condense-by-objs (group-relations relations))))
) ; END form-ans-descr


(defun form-ans-time (relations)
; ````````````````````````````````
; Creates a ULF from a list of times.
;
  (make-set relations)
) ; END form-ans-time


(defun relations-empty? (relations)
; ```````````````````````````````````
; Checks if a list of relations is empty (i.e. an atomic element equal to 'None).
;
  (and (symbolp relations) (equal relations 'None))
) ; END relations-empty?


(defun relations-singular? (relations)
; ````````````````````````````````````
; Checks if a list of relations only has a singular element.
;
  (and (listp relations) (= (length relations) 1))
) ; END relations-singular?


(defun relations-some? (relations)
; `````````````````````````````````
; Checks if a list of relations has a few elements (i.e. 5 or less, but greater than one).
;
  (and (listp relations) (> (length relations) 1) (<= (length relations) 5))
) ; END relations-some?


(defun relations-many? (relations)
; `````````````````````````````````
; Checks if a list of relations has many elements (i.e. more than 5).
;
  (and (listp relations) (> (length relations) 5))
) ; END relations-many?


(defun compare-certainty (relation)
; ```````````````````````````````````
; Compares certainty of relation to the global threshold parameter.
;
  (> (second relation) *certainty-threshold*)
) ; END compare-certainty


(defun filter-relations (relations filter)
; ``````````````````````````````````````````
; Given a list of relations, filter so only the specific relations in filter are kept.
;
  (let ((filtered (remove-if-not (lambda (rel) (member (second (first rel)) filter)) relations)))
    (if filtered filtered 'None))
) ; END filter-relations


(defun certain-rels (relations)
; ````````````````````````````````````
; Retrieves relations from a list of relations with certainties above the threshold.
;
  (remove nil (mapcar (lambda (rel)
    (if (compare-certainty rel) (car rel))) relations))
) ; END certain-rels


(defun uncertain-rels (relations)
; ````````````````````````````````````
; Retrieves relations from a list of relations with certainties below the threshold.
;
  (remove nil (mapcar (lambda (rel)
    (if (not (compare-certainty rel)) (car rel))) relations))
) ; END uncertain-rels


(defun group-relations (relations)
; `````````````````````````````````
; Groups relations into sublists according to predicate.
;
  (when (equal relations 'None) (return-from group-relations 'None))
  (let ((i -1) preps result)
    (mapcar (lambda (rel)
      (let ((prep (car (member rel preps :test (lambda (r p) (equal (second rel) (first p)))))))
        (cond
          (prep (let* ((n (second prep)) (prev (nth n result)))
            (setq result (replace-n n result (cons rel prev)))))
          (t (setq i (1+ i))
             (setq preps (cons (list (second rel) i) preps))
             (setq result (append result (list (list rel))))))))
      relations)
  result)
) ; END group-relations


(defun condense-by-objs (relations)
; ```````````````````````````````````
; Given a list of relations (assumed to have the same subject and predicate), condense relations into a
; single relation with a (set-of ...) object.
; NOTE: we make an exception for "between" (or any other predicates with more than one object). Certainties
; are ignored at this point.
;
  (let ((pred (second (car relations))) (subj (first (car relations))) (objs (make-plur-np-obj relations)))
    (if (equal pred 'BETWEEN.P)
      (mapcar (lambda (rel)
        `(,(first rel) ,(second rel) (set-of ,(make-np (third rel) 'block.n) ,(make-np (fourth rel) 'block.n)))) relations)
     `(,subj ,pred ,objs)))
) ; END condense-by-objs


(defun make-plur-np-obj (relations)
; ``````````````````````````````````
; Makes plural np with conjoined object names as modifier
;
  (make-np (make-set (remove-duplicates (remove nil
    (mapcar (lambda (rel) (third rel)) relations)) :test #'equal)) 'block.n)
) ; END make-plur-np-obj


(defun get-color (name)
; ```````````````````````
; Gets the color of a given name.
; e.g. (get-color '|Twitter|) => red.a
;
  (let ((color-prop (car (remove-if-not #'color-prop? (get-from-context name)))))
    (caadr color-prop))
) ; END get-color


(defun make-np (name type)
; ``````````````````````````
; Forms a definite noun phrase from a name and type.
; e.g. (make-np '|Twitter| 'block.n) => (the.d (|Twitter| block.n))
;
  (if (listp name) `(the.d (,name (plur ,type)))
    `(the.d (,name ,type)))
) ; END make-np


(defun make-color-np (name type)
; ```````````````````````````````
; Forms an indefinite noun phrase from a color adjective and type.
; e.g. (make-color-np '|Twitter| 'block.n) => (a.d (red.a block.n))
;
  (let ((color (get-color name)))
    `(a.d (,color block.n)))
) ; END make-color-np


(defun make-set (list)
; `````````````````````
; Makes a set ULF (SET-OF ...) of the elements of a list, if multiple elements.
; If list has only a single element, just return that element.
;
  (if (<= (length list) 1) (car list) (cons 'SET-OF list))
) ; END make-set


; TTT flags and other preds are defined as follows
; ``````````````````````````````````````````````````
(defun yn-flag? (p)
  (ttt:match-expr '(! yn-word? (tense? yn-word?) ((tense? aspect?) yn-word?) ((tense? aspect?) (yn-word? _*))
                      ((tense? aspect?) (pasv yn-word?)) ((tense? aspect?) ((pasv yn-word?) _*))) p))

(defun count-flag? (p)
  (ttt:match-expr '(! (HOW.MOD-A MANY.A)) p))

(defun ident-prep-flag? (p)
  (ttt:match-expr '(prep? (! WHAT.PRO WHICH.PRO (WHAT.D _!) (WHICH.D _!))) p))

(defun ident-flag? (p)
  (ttt:match-expr '(! WHAT.PRO WHICH.PRO (WHAT.D _!) (WHICH.D _!)) p))

(defun descr-flag? (p)
  (ttt:match-expr '(! (AT.P (WHAT.D PLACE.N))) p))

(defun time-flag? (p)
  (ttt:match-expr '(! (WHAT.D time-word?)) p))

(defun exist-flag? (p)
  (ttt:match-expr '(! THERE.PRO) p))

(defun color-object-flag? (p)
  (ttt:match-expr '(WHAT.D (COLOR.A _!)) p))

(defun color-flag? (p)
  (ttt:match-expr '(OF.P (WHAT.D (! COLOR.N (PLUR COLOR.N)))) p))

(defun yn-word? (p)
  (member p '(BE.V DO.AUX-S DO.AUX-V CAN.AUX-S CAN.AUX-V)))

(defun time-word? (p)
  (member p '(TURN.N TIME.N STAGE.N STEP.N QUESTION.N ITERATION.N MOVE.N PERIOD.N)))

(defun color-word? (p)
  (member p '(RED.A ORANGE.A YELLOW.A GREEN.A BLUE.A PURPLE.A PINK.A WHITE.A BLACK.A MAGENTA.A GRAY.A GREY.A VIOLET.A INDIGO.A BROWN.A)))

(defun qmark? (p)
  (equal p '?))

(defun poss-ques? (ulf)
  (and (listp ulf) (equal (car ulf) 'poss-ques)))