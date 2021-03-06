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
  (let* ((ulf-base (uninvert-question (remove-not (remove-adv-e (remove-adv-f ulf)))))
         (coords (extract-coords object-locations))
         (where-question (extract-where-question ulf))
         (when-question (extract-when-question ulf))
         (neg (extract-neg ulf))
         (freq (extract-freq ulf))
         (subj (extract-subj ulf-base))
         (obj (extract-obj ulf-base))
         (relation (extract-relation ulf-base))
         (action (extract-action ulf-base))
        ;;  (quant-times (get-referred-times coords ulf ulf-base where-question when-question))
        ;;  (quantifier (first quant-times)) (times (second quant-times))
         quantifier (times (get-referred-times coords ulf ulf-base where-question when-question))
         ans)
    (format t "where question: ~a~%" where-question)
    (format t "when question: ~a~%" when-question)
    (format t "neg: ~a~%" neg)
    (format t "freq: ~a~%" freq)
    (format t "base ulf: ~a~%" ulf-base)
    (format t "subj: ~a~%" subj)
    (format t "obj: ~a~%" obj)
    (format t "extracted relation: ~a~%" relation)
    (format t "extracted action-verb: ~a~%" action)
    (format t "blocks at coordinates: ~a~%" coords)
    (format t "quantifier + referred times: ~a ~a~%" quantifier times) ; DEBUGGING

    ; Detect different types of historical questions which might be asked. Some are straightforward spatial questions
    ; but just asked about the past, whereas others have to do with the actions themselves, e.g. "what blocks did I move?"
    ; TODO: This is somewhat ad-hoc and likely needs to be generalized.
    (cond
      ; If asking a where-question
      (where-question
        (apply-to-times `(compute-relations ,subj ,coords) times quantifier when-question))
      ; If asking about action plus relation, e.g. "what block did I put on the Twitter block"
      ((and relation action)
        (apply-to-times `(compute-move+relation ,relation ,coords ,obj ,neg) times quantifier when-question))
      ; If asking about spatial relation
      (relation
        (apply-to-times `(compute-relation ,relation ,coords ,neg) times quantifier when-question))
      ; Otherwise assume the question is about block moves
      (t
        (apply-to-times `(compute-move ,obj ,neg) times quantifier when-question)))
  )
) ; END recall-answer


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


(defun extract-freq (ulf)
; `````````````````````````
; Extracts any frequency modifier from the ULF (or nil if none exist).
; Returns either a count, i.e. number, or a quantifier such as 'always.
;
 (let ((freq (ttt:apply-rules '(
      (/ (^* (adv-f (_!1 (numerical-det? _!2)))) (numerical-det! numerical-det?))
      (/ (^* once.adv-f) 1)
      (/ (^* twice.adv-f) 2)
      (/ (^* never.adv-f) always); 'never' is 'always' + negative polarity
      (/ (^* always.adv-f) always))
    ulf :shallow t)))
  (if (atom freq) freq))
) ; END extract-freq


(defun get-referred-times (coords ulf ulf-base where-question when-question)
; ````````````````````````````````````````````````````````````````````````````
;
;
  (let ((adv-e-list (collect-adv-e ulf)) times)
    (setq times (get-times-from-adv-e-list coords adv-e-list))
    (format t "~a~%" times)
  )
) ; END get-referred-times


(defun collect-adv-e (ulf)
; `````````````````````````
; Creates a list of adv-e words or phrases (including sentencial prepositions) in the ULF.
;
  (let (adv-e-list)
    (labels ((collect-adv-e-recur (ulf-part)
        (cond
          ; If ULF is an adv-e of some sort, append to adv-e-list
          ((and (atom ulf-part) (adv-e-lex? ulf-part))
            (setq adv-e-list (cons ulf-part adv-e-list)))
          ((ttt:match-expr '(adv-a-lex? adv-e-lex?) ulf-part)
            (setq adv-e-list (cons ulf-part adv-e-list)))
          ((time-adverbial-phrase? ulf-part)
            (setq adv-e-list (cons ulf-part adv-e-list)))
          ; Otherwise, if ULF is atom, do nothing
          ((atom ulf-part) nil)
          ; Otherwise, if ULF is list, recur on each sub-part
          (t (mapcar #'collect-adv-e-recur ulf-part)))))
      (collect-adv-e-recur ulf))
    (mapcar #'apply-sub-macro adv-e-list))
) ; END collect-adv-e


(defun get-times-from-adv-e-list (coords adv-e-list)
; ```````````````````````````````````````````````````
; NOTE: if where/when question, use most-recent.
;
  (let ((times (get-times-all)))
    ; We want to iterate through each adv-e and "whittle down" the times being referred to.
    (dolist (adv-e adv-e-list)
      (let ((new-times (get-times-from-adv-e coords times adv-e)))
        (cond
          ((equal new-times '(None))
            (setq times nil))
          (new-times
            (setq times new-times))
        ))
    )
  )
) ; END get-times-from-adv-e-list


(defun get-times-from-adv-e (coords times adv-e)
; ```````````````````````````````````````````````
; 
;
  (cond
    ; If lexical adv-e, get modified list of times
    ((adv-e-lex? adv-e)
      (resolve-time-adv-e-lex! times adv-e))
    ; If adv-e is some other atom, do nothing
    ((atom adv-e) nil)
    ; If adv-e is premodified by some adv-a (e.g. "directly before ..."), apply adv-a
    ; to list of times obtained from adv-e
    ((adv-a-lex? (first adv-e))
      (funcall (resolve-time-adv-a! (first adv-e)) (get-times-from-adv-e times (second adv-e))))
    ; If adv-e is sentential preposition, resolve to list of times
    ((sent-prep? coords (first adv-e))
      (resolve-time-psp! adv-e))
    ; If adv-e is a conjunction of prepositional phrases, find intersection of times from both
    ((prep-conjunction? (second adv-e))
      (intersection (resolve-time-pp! coords (first (second adv-e))) (resolve-time-pp! coords (third (second adv-e)))))
    ; Otherwise, resolve prepositional phrase to list of times
    ((prep-phrase? (second adv-e))
      (resolve-time-pp! coords (second adv-e))))
) ; END get-times-from-adv-e


(defun resolve-time-adv-e-lex! (times adv-e)
; ```````````````````````````````````````````
; Resolves an adv-e lexical item to a list of times.
;
  (cond
    ((hist-adv-cur? adv-e) (list *time*))
    ((hist-adv-prev? adv-e) (get-times-before *time*))
    ((hist-adv-next? adv-e) (get-times-after *time*))
    ((hist-adv-init? adv-e) (least-recent times))
    ((hist-adv-just? adv-e) (most-recent times))
    ((hist-adv-recent? adv-e) (most-recent times 4))
    ((hist-adv-ever? adv-e) times))
) ; END resolve-time-adv-e-lex!


(defun resolve-time-adv-a! (adv-a)
; ``````````````````````````````````
; Resolves an adv-a lexical item (i.e. "right" in "right before ...") to a function over a list of times.
; 
  (cond
    ((hist-adv-directly? adv-a)
      #'most-recent))
) ; END resolve-time-adv-a!


(defun resolve-time-psp! (coords ps)
; ``````````````````````````````````
; Resolves the sentence within a sentential predicate phrase to a list of times.
;
  (let ((ps-prep (first ps)) (ps-sent (second ps)) times)
    ; Get time(s) at which ps-sent holds true
    (setq times (get-time-ulf-true coords ps-sent))
    ; Apply sentential preposition to time(s)
    (cond
      ((equal times '(None)) times)
      ((hist-ps-prev? ps-prep) (get-times-before-set times))
      ((hist-ps-next? ps-prep) (get-times-after-set times))
      ((hist-ps-while? ps-prep) times)))
) ; END resolve-time-psp!


(defun resolve-time-pp! (coords pp)
; ``````````````````````````````````
; Resolves a prepositional phrase to a list of times.
;
  (let ((pp-prep (first pp)) (pp-np (second pp)))

  )
) ; END resolve-time-pp!



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
      ; No satisfying ref times
      ((equal (car ref-times) 'None)
        ref-times)
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




;; (defun get-referred-times (coords ulf ulf-base where-question when-question)
;; ; ````````````````````````````````````````````````````````````````````````````
;; ; Given a historical question ULF, get the list of times referred to by the ULF (for instance,
;; ; by the adv-e phrases/lexical words), and a quantifier over those times (by default, most-recent)
;; ; TODO: the whole quantifier selection is really messy and definitely needs to be rethought.
;; ; 
;;   (let (adv-e-phrase adv-e-word quantifier times-phrase times-word times)
;;     ; Retrieve any adv-e phrase and adv-e lexical word in ulf
;;     (setq adv-e-phrase (extract-adv-e-phrase ulf))
;;     (setq adv-e-word (extract-adv-e-word ulf))
;;     ; Resolve adv-e phrase and adv-e word to times (or quantifier over times in
;;     ; the case of the latter)
;;     ;; (format t "adv-e-phrase: ~a~%" adv-e-phrase)
;;     ;; (format t "adv-e-word: ~a~%" adv-e-word) ; DEBUGGING
;;     (when adv-e-phrase
;;       (setq times-phrase (get-times-from-adv-e-phrase coords adv-e-phrase)))
;;     (if (equal (car times-phrase) 'None) (setq times-phrase nil))
;;     (when adv-e-word
;;       (setq times-word (get-times-from-adv-e-word coords adv-e-word)))
;;     (format t "times-phrase: ~a~%" times-phrase)
;;     (format t "times-word: ~a~%" times-word) ; DEBUGGING

;;     ; When-question acts as an adv-e phrase referring to all times
;;     (when when-question
;;       (setq times-phrase (time-inclusive (get-times-before *time* -1))))

;;     (cond
;;       ; If lexical adv-e gives some special quantifier
;;       ((and times-word (not (listp times-word)))
;;         (setq quantifier times-word)
;;         (setq times times-phrase))
;;       ; If both phrasal adv-e and lexical adv-e give list of times
;;       ((and times-word times-phrase)
;;         (setq times (intersection times-word times-phrase)))
;;       ; If only lexical adv-e gives list of times
;;       (times-word (setq times times-word))
;;       ; If only phrasal adv-e gives list of times
;;       (times-phrase (setq times times-phrase)))

;;     ; For where-questions, it makes sense to use most-recent quantifier.
;;     (when where-question
;;       (setq quantifier 'most-recent))

;;     ; For simple questions about actions (e.g. "what block(s) did I move?"), the quantifier also seems to
;;     ; depend on the plurality of the subject, i.e. "what blocks did I move" looks over all times (if no
;;     ; adv-e is specified, the times between the previous utterance and the current utterance), whereas
;;     ; "what block did I move" looks only at the most recent time (since times correspond to individual moves).
;;     (cond
;;       ; Plural or "how many"
;;       ((or (ttt:match-expr '(_! ((tense? action-verb?) (! wh-pron? (det? (^* (plur noun?)))) _*)) ulf-base)
;;            (ttt:match-expr '(_! ((tense? aspect?) (action-verb? (! wh-pron? (det? (^* (plur noun?)))) _*))) ulf-base)
;;            (ttt:match-expr '(_! ((tense? action-verb?) ((nquan (how.mod-a many.a)) (plur noun?)) _*)) ulf-base))
;;         (if (and (not times) (not quantifier)) (setq times (get-times-before *time* (diff-times *time* *time-prev*)))
;;           (setq times (time-inclusive times)))
;;         (if (not quantifier) (setq quantifier 'ever)))
;;       ; Singular
;;       ((ttt:match-expr '(_! ((tense? action-verb?) (det? (^* noun?)) _*)) ulf-base)
;;        (ttt:match-expr '(_! ((tense? aspect?) (action-verb? (det? (^* noun?)) _*))) ulf-base)
;;         (if (and (not times) (not quantifier)) (setq times (get-times-before *time* 1))
;;           (setq times (time-inclusive times)))
;;         (if (not quantifier) (setq quantifier 'most-recent))))
;;     ; By default, look at all times before now, and quantifier is just the most recent.
;;     (if (not times) (setq times (get-times-before *time* -1)))
;;     (if (not times) (setq times (list *time*)))
;;     (if (not quantifier) (setq quantifier 'most-recent))

;;   (list quantifier times))
;; ) ; END get-referred-times


;; (defun extract-adv-e-phrase (ulf)
;; ; `````````````````````````````````
;; ; Extracts an adv-e phrase from a ULF, and applies any sub macros in the phrase.
;; ;
;;   (let ((adv-e-phrase
;;       (ttt:apply-rule '(/ (^* (adv-e _!)) (adv-e _!))
;;         (apply-sub-macro ulf) :shallow t)))
;;     (if (adv-e? adv-e-phrase) adv-e-phrase nil))
;; ) ; END extract-adv-e-phrase


;; (defun extract-adv-e-word (ulf)
;; ; `````````````````````````````````
;; ; Extracts an adv-e word from a ULF.
;; ;
;;   (let ((adv-e-word (ttt:apply-rule '(/ (^* adv-e-lex?) adv-e-lex?) ulf :shallow t)))
;;     (if (adv-e? adv-e-word) adv-e-word nil))
;; ) ; END extract-adv-e-phrase


;; (defun get-times-from-adv-e-word (coords ulf)
;; ; ````````````````````````````````````````````
;; ; Resolves a lexical adv-e into either a list of times (for example,
;; ; something like "previously"), or an operator over times (e.g. "ever").
;; ;
;;   (let ((time-funcall (ttt:apply-rules
;;       `((/ hist-adv-prev? (get-times-before ,*time* -1))
;;         (/ hist-adv-next? (get-times-after ,*time* -1))
;;         (/ hist-adv-init? (get-times-init 1))
;;         (/ hist-adv-cur? (list ,*time*))
;;         (/ hist-adv-recent? (identity most-recent))
;;         (/ hist-adv-just? (identity most-recent))
;;         (/ hist-adv-always? (identity always))
;;         (/ hist-adv-ever? (identity ever))
;;         (/ hist-adv-never? (identity not-ever))
;;         (/ (! symbol? (not-fbound? _*)) (identity nil)))
;;       ulf :shallow t)))
;;     (apply (car time-funcall) (cdr time-funcall)))
;; ) ; END get-times-from-adv-e-word


;; (defun get-times-from-adv-e-phrase (coords ulf)
;; ; ```````````````````````````````````````````````
;; ; Resolves a phrasal adv-e (e.g. "before I moved it") into a list of times.
;; ;
;;   (let ((adv-e (cadr (extract-adv-e-phrase ulf))))
;;     (cond
;;       ((prep-conjunction? adv-e)
;;         (intersection (get-times-from-pp coords (first adv-e)) (get-times-from-pp coords (third adv-e))))
;;       (t (get-times-from-pp coords adv-e))))
;; ) ; END get-times-from-adv-e-phrase


;; (defun get-times-from-pp (coords ulf)
;; ; `````````````````````````````````````
;; ; Gets a list of times corresponding to a prepositional phrase,
;; ; given the embedded noun phrase or reified event.
;; ; NOTE: this function could use some cleaning.
;; ;
;;   (let* ((adv-a (if (hist-adv-directly? (first ulf)) (first ulf)))
;;          (prep (progn (when adv-a (setq ulf (second ulf))) (first ulf)))
;;          ; Get reference times denoted by the reified event/noun phrase, which are
;;          ; to be modified by the preposition.
;;          (ref-times (cond
;;           ((reified-event? (second ulf))
;;             (get-times-from-ke coords (second ulf)))
;;           ((indexical-np? (second ulf))
;;             (list *time*))
;;           ((quant-np? (second ulf))
;;             (last (get-times-before *time*
;;               (numerical-det! (first (second ulf))))))
;;           ((definite-np? (second ulf))
;;             (get-times-from-np coords (second ulf))))))
;;     ; Apply predicate operation (before, after, during), along with any modifying
;;     ; adverb (e.g. "right before").
;;     (cond
;;       ; No satisfying ref times
;;       ((equal (car ref-times) 'None)
;;         ref-times)
;;       ; "just before"
;;       ((and adv-a (hist-prep-prev? prep)) (set-difference (union1 (mapcar
;;         (lambda (ref-time) (get-times-before ref-time 1)) ref-times)) ref-times))
;;       ; "before"
;;       ((hist-prep-prev? prep)
;;         (set-difference (get-times-before (car ref-times) -1) ref-times))
;;       ; "just after"
;;       ((and adv-a (hist-prep-next? prep)) (set-difference (union1 (mapcar
;;         (lambda (ref-time) (get-times-after ref-time 1)) ref-times)) ref-times))
;;       ; "after"
;;       ((hist-prep-next? prep)
;;         (set-difference (get-times-after (car ref-times) -1) ref-times))
;;       (t ref-times)))
;; ) ; END get-times-from-pp


;; (defun get-times-from-ke (coords ulf)
;; ; `````````````````````````````````````
;; ; Gets a list of times corresponding to a reified event.
;; ;
;;   (let ((ke-funcall (ttt:apply-rules
;;       `(
;;         (/ (^* ((past action-verb?) (det? (nnp? noun?)) (prep? (det? (nnp?2 noun?)))))
;;            (get-time-of-move+relation ,coords (nnp? prep? nnp?2)))
;;         (/ (^* ((past action-verb?) (det? (nnp? noun?))))
;;            (get-time-of-move nnp?))
;;         (/ (ke ((det? (nnp?1 noun?)) ((past be.v) (prep? (det? (nnp?2 noun?))))))
;;            (get-time-of-relation ,coords (nnp?1 prep? nnp?2)))
;;         (/ (ke _*) (identity nil)))
;;       ulf)) ke-result)
;;     (setq ke-result (apply (car ke-funcall) (cdr ke-funcall)))
;;     (if ke-result (list ke-result) (list 'None)))
;; ) ; END get-times-from-ke


;; (defun get-times-from-np (coords ulf)
;; ; `````````````````````````````````````
;; ; Gets a list of times corresponding to a definite noun phrase.
;; ; TODO: This will need updating once support for n+preds (e.g. "the
;; ; turn where I moved the SRI block") is added.
;; ;
;;   (let ((time-funcall (ttt:apply-rules
;;       `((/ (det? (adj? hist-noun-turn?)) (hist-adj! adj? 1))
;;         (/ (det? (adj? (? numerical-adj?) (plur hist-noun-turn?))) (hist-adj! adj? ?))
;;         (/ (det? (adj? hist-noun-prev?)) (get-times-before ,*time* 3))
;;         (/ (det? (adj? hist-noun-next?)) (get-times-after ,*time* 3))
;;         (/ (det? hist-noun-prev?) (get-times-before ,*time* -1))
;;         (/ (det? hist-noun-next?) (get-times-after ,*time* -1))
;;         (/ (det? hist-noun-init?) (get-times-init 1))
;;         (/ (not-fbound? _*) (identity nil)))
;;       ulf)))
;;     (apply (car time-funcall) (cdr time-funcall)))
;; ) ; END get-times-from-np


;; (defun hist-adj! (adj &optional num)
;; ; ````````````````````````````````````
;; ; Maps a temporal adjective + n ("window size" of time period) to a
;; ; function application to retrieve corresponding (discrete) times.
;; ;
;;   (let ((n (cond ((null num) 3) ((numberp num) num) (t (numerical-adj! num)))))
;;     (ttt:apply-rules
;;     `((/ hist-adj-prev? (get-times-before ,*time* ,n))
;;       (/ hist-adj-next? (get-times-after ,*time* ,n))
;;       (/ hist-adj-init? (get-times-init ,n))
;;       (/ hist-adj-final? (get-times-final ,n))
;;       (/ hist-adj-cur? (list ,*time*))
;;       (/ numerical-adj? (get-time-nth ,(numerical-adj! adj))))
;;     adj))
;; ) ; END hist-adj!


(defun reconstruct-scene (coords Tn)
; `````````````````````````````````````
; Reconstruct the scene (i.e. a list of coordinates for each block)
; at the time denoted by Tn, given current coordinates.
; coords should be a list of coordinates in the simplified form (|Name| ?x ?y ?z)
;
  (let* ((Ti (get-prev-time *time*)) (scene coords) moves)
    (loop while (and Ti Tn (> (compare-time Ti Tn) -1) (> (compare-time Ti 'NOW0) -1)) do
      (setq moves (extract-moves (gethash Ti *context*)))
      (mapcar (lambda (move)
        (setq scene (subst (first move) (second move) scene :test #'~equal))) moves)
      (setq Ti (get-prev-time Ti)))
    scene)
) ; END reconstruct-scene


(defun form-pred-list (coords-list1 prep-list coords-list2 &key neg)
; ```````````````````````````````````````````````````````````````````
; Form predicates from all relations that are satisfied having
; things from coords-list1 as the subject, a preposition from
; prep-list, and coords-list2 as the object.
; If neg is given as t, return negated predicates.
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


(defun compute-move+relation (Ti rel coords obj neg)
; ````````````````````````````````````````````````````
; Computes all moves of a block into a particular relation, i.e. all whether, at time Ti,
; the given object was moved, and that the relation holds in time Ti+1. If neg is given,
; either the object was not moved, or the relation didn't hold in time Ti+1.
;
  (let ((moves (get-moves-car-form Ti)) (relations (compute-relation (get-next-time Ti) rel coords nil)))
    ; Get rid of any moves for which the relation does not hold in time Ti+1
    (setq moves (remove-if-not (lambda (move) (find move relations
                 :test (lambda (x y) (equal (car x) (car y))))) moves))
    ; If negation, find complement of moves
    (if neg (setq moves (negate-moves moves)))
    ; Simplify form of relations and filter based on obj
    (filter+process-moves moves obj))
) ; END compute-move+relation


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
    (form-pred-list coords-list1 (list prep) coords-list2 :neg neg))
) ; END compute-relation


(defun compute-move (Ti obj neg)
; ```````````````````````````````
; Computes all moves at a particular time with the given object (may be a variable with/without
; restrictors).
;
  (let* ((moves (get-moves-car-form Ti)))
    ; If negation, we need to access context to see all blocks (or more generally, 'movable entities'),
    ; and remove all of the blocks which moved during Ti.
    (if neg (setq moves (negate-moves moves)))
    ; Simplify form of relations and filter based on obj
    (filter+process-moves moves obj))
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


(defun get-moves-car-form (Ti)
; `````````````````````````````
; Gets the moves at time Ti in a form with block name as car.
;
  (mapcar (lambda (move) (list (caar move) (cdar move) (cdadr move)))
    (extract-moves (gethash Ti *context*)))
) ; END get-moves-car-form


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
;
  ; Apply function to each time, and create a list of the time paired with all returned relation
  (let* ((time-rels (sort (copy-seq (remove-if (lambda (x) (null (second x)))
                (mapcar (lambda (time) (list time (apply (car f) (cons time (cdr f))))) times)))
              (lambda (x y) (> (compare-time (car x) (car y)) 0))))
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
        (intersection1 answers))))
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


(defun get-time-ulf-true (coords ulf)
; ````````````````````````````````````
; Gets the time at which a ULF (simple proposition, e.g. "I moved the Twitter block") holds true.
;
  (let (ps-result
      (ps-funcall (ttt:apply-rules `(
        ; before I put the Twitter block on the Texaco block
        (/ (^* ((past action-verb?) (det? (nnp? noun?)) (prep? (det? (nnp?2 noun?)))))
          (get-time-of-move+relation ,coords (nnp? prep? nnp?2)))
        ; before I moved the Twitter block
        (/ (^* ((past action-verb?) (det? (nnp? noun?))))
          (get-time-of-move nnp?))
        ; before the Twitter block was on the Texaco block
        (/ ((det? (nnp?1 noun?)) ((past be.v) (prep? (det? (nnp?2 noun?)))))
          (get-time-of-relation ,coords (nnp?1 prep? nnp?2)))
        (/ (not-fbound? _*) (identity nil)))
      ulf)))
    (setq ps-result (apply (car ps-funcall) (cdr ps-funcall)))
    (if ps-result (list ps-result) (list 'None)))
) ; END get-time-ulf-true


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


(defun get-time-of-move+relation (coords rel)
; `````````````````````````````````````````````
; Gets the most recent time at which an object of a given name was moved into a relation
; with another object of a given name.
;
  (labels ((get-time-of-move+relation-recur (rel Ti)
      (let* ((scene (reconstruct-scene coords (get-next-time Ti)))
             (name1 (first rel)) (prep (second rel)) (name2 (third rel))
             (coords1 (find-car name1 scene)) (coords2 (find-car name2 scene))
             (rel-true (eval-relation-bool prep coords1 coords2))
             (moves (extract-moves (gethash Ti *context*)))
             (moved-blocks (mapcar #'caar moves)))
        (cond
          ((null Ti) nil)
          ((and (member name1 moved-blocks) rel-true) Ti)
          ((equal Ti 'NOW0) nil)
          (t (get-time-of-move-recur name (get-prev-time Ti)))))))
    (get-time-of-move-recur name (get-prev-time *time*)))
) ; END get-time-of-move+relation


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


(defun get-time-n-moves-before (Ti n)
; ````````````````````````````````````
; Gets the time of the nth most recent move.
;
  (labels ((get-time-n-moves-before-recur (Tj j)
      (let* ((moves (extract-moves (gethash Tj *context*)))
             (k (- j (length moves))))
        (cond
          ((null Tj) nil)
          ((<= k 0) Tj)
          ((equal Tj 'NOW0) nil)
          (t (get-time-n-moves-before-recur (get-prev-time Tj) k))))))
    (get-time-n-moves-before-recur (get-prev-time Ti) n))
) ; END get-time-n-moves-before


(defun get-time-n-moves-after (Ti n)
; ````````````````````````````````````
; Gets the time of the nth most recent move.
;
  (labels ((get-time-n-moves-after-recur (Tj j)
      (let* ((moves (extract-moves (gethash Tj *context*)))
             (k (- j (length moves))))
        (cond
          ((null Tj) nil)
          ((<= k 0) Tj)
          ((equal Tj 'NOW0) nil)
          (t (get-time-n-moves-after-recur (get-next-time Tj) k))))))
    (get-time-n-moves-after-recur (get-next-time Ti) n))
) ; END get-time-n-moves-after


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