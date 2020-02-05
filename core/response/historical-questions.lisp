;; Dec 9/19
;; ================================================
;;
;; Functions used in answering historical questions
;;

; T0
; "Where is the Target block?"
; ((|Target| at-loc.p ($ loc 0 0 0)) (|Starbucks| at-loc.p ($ loc 1 0 0)) (|Twitter| at-loc.p ($ loc 2 0 0)) (|Texaco| at-loc.p ($ loc 3 0 0)) (|McDonalds| at-loc.p ($ loc 4 0 0)) (|Mercedes| at-loc.p ($ loc 5 0 0)) (|Toyota| at-loc.p ($ loc 6 0 0)) (|Burger King| at-loc.p ($ loc 7 0 0))  )
; T1
; [ans]
; *Moves Starbucks, Target blocks*
; "Where is the Target block?"
; ((|Target| at-loc.p ($ loc 5 5 1)) (|Starbucks| at-loc.p ($ loc 5 5 0)) (|Twitter| at-loc.p ($ loc 2 0 0)) (|Texaco| at-loc.p ($ loc 3 0 0)) (|McDonalds| at-loc.p ($ loc 4 0 0)) (|Mercedes| at-loc.p ($ loc 5 0 0)) (|Toyota| at-loc.p ($ loc 6 0 0)) (|Burger King| at-loc.p ($ loc 7 0 0))      (|Starbucks| ((past move.v) (from.p-arg ($ loc 1 0 0)) (to.p-arg ($ loc 5 5 0)))) (|Target| ((past move.v) (from.p-arg ($ loc 0 0 0)) (to.p-arg ($ loc 5 5 1)))))
; T2
; [ans]
; *Moves Target, Twitter blocks*
; "Where was the Target block before I moved it?"
; ((|Target| at-loc.p ($ loc 2 0 1)) (|Starbucks| at-loc.p ($ loc 5 5 0)) (|Twitter| at-loc.p ($ loc 5 5 1)) (|Texaco| at-loc.p ($ loc 3 0 0)) (|McDonalds| at-loc.p ($ loc 4 0 0)) (|Mercedes| at-loc.p ($ loc 5 0 0)) (|Toyota| at-loc.p ($ loc 6 0 0)) (|Burger King| at-loc.p ($ loc 7 0 0))      (|Target| ((past move.v) (from.p-arg ($ loc 5 5 1)) (to.p-arg ($ loc 2 0 1)))) (|Twitter| ((past move.v) (from.p-arg ($ loc 2 0 0)) (to.p-arg ($ loc 5 5 1)))))
; T3
; [ans]


; "Where did I move the Texaco block" => "Where was the Texaco block after I moved it"




(defun recall-answer (object-locations ulf)
; ````````````````````````````````````````````
; Given current observed block locations and the ULF of the query, recall the answer by consulting
; historical record of block moves stored in context.
;
  (format t "object locations: ~a~%" object-locations) ; DEBUGGING
  (let ((coords (extract-coords object-locations)))
    (format t "blocks at coordinates: ~a~%" coords) ; DEBUGGING

    '((|Texaco| to-the-left-of.p |Twitter|))
  )
) ; END recall-answer


(defun extract-adv-e-phrase (ulf)
; `````````````````````````````````
; Extracts an adv-e phrase from a ULF, and applies any sub macros in the phrase.
;
  (nth-value 1 (ulf-lib:apply-sub-macro (ttt:apply-rule '(/ (^* (adv-e _!)) (adv-e _!)) ulf
                  :shallow t) :calling-package *package*))
) ; END extract-adv-e-phrase


; (ADV-E (DURING.P (THE.D (N+PREDS TURN.N ()))))
; (WHEN.PS (I.PRO ((PRES MOVE.V) (THE.D (|Twitter| BLOCK.N)))))


(defun get-times-from-adv-e-word (ulf)
; ``````````````````````````````````````
;
;
) ; END get-times-from-adv-e-word


(defun get-times-from-adv-e-phrase (ulf)
; ```````````````````````````````````````
; Resolves a phrasal adv-e (e.g. "before I moved it") into a list of times.
;
  (let ((adv-e (cadr (extract-adv-e-phrase ulf))))
    (cond
      ((prep-conjunction? adv-e)
        (intersection (get-times-from-pp (first adv-e)) (get-times-from-pp (third adv-e))))
      (t (get-times-from-pp adv-e))))
) ; END get-times-from-adv-e-phrase


(defun get-times-from-pp (ulf)
; ``````````````````````````````
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
            (get-times-from-ke (second ulf)))
          ((indexical-np? (second ulf))
            (list *time*))
          ((quant-np? (second ulf))
            (last (get-times-before *time*
              (numerical-det! (first (second ulf))))))
          ((definite-np? (second ulf))
            (get-times-from-np (second ulf))))))
    ; Apply predicate operation (before, after, during), along with any modifying
    ; adverb (e.g. "right before").
    (cond
      ((and adv-a (hist-prep-prev? prep)) (set-difference (union1 (mapcar
        (lambda (ref-time) (get-times-before ref-time 1)) ref-times)) ref-times))
      ((hist-prep-prev? prep)
        (set-difference (get-times-before (car ref-times) -1) ref-times))
      ((and adv-a (hist-prep-next? prep)) (set-difference (union1 (mapcar
        (lambda (ref-time) (get-times-after ref-time 1)) ref-times)) ref-times))
      ((hist-prep-next? prep)
        (set-difference (get-times-after (car ref-times) -1) ref-times))
      (t ref-times)))
) ; END get-times-from-pp


(defun get-times-from-ke (ulf)
; ``````````````````````````````
; Gets a list of times corresponding to a reified event.
;
  (let ((ke-funcall (ttt:apply-rules
      `((/ (^* ((past move.v) (det? (nnp? noun?)))) (get-time-of-move nnp?)))
      ulf)))
    (list (apply (car ke-funcall) (cdr ke-funcall))))
) ; END get-times-from-ke


(defun get-times-from-np (ulf)
; `````````````````````````````
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
        (/ (det? hist-noun-init?) (get-times-init 1)))
      ulf)))
    (apply (car time-funcall) (cdr time-funcall)))
) ; END get-times-from-np


(defun hist-adj! (adj &optional num)
; ````````````````````````````````````
; Maps a temporal adjective + n ("window size" of time period) to a
; function application to retrieve corresponding (discrete) times.
;
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


(defun reconstruct-scene (coords Tn)
; `````````````````````````````````````
; Reconstruct the scene (i.e. a list of coordinates for each block)
; at the time denoted by Tn, given current coordinates.
; coords should be a list of coordinates in the simplified form (|Name| ?x ?y ?z)
;
  (let* ((Ti (get-prev-time *time*)) (scene coords) moves)
    (loop while (and Ti (> (compare-time Ti Tn) -1) (> (compare-time Ti 'T0) -1)) do
      (setq moves (extract-moves (gethash Ti *context*)))
      (mapcar (lambda (move)
        (setq scene (subst (first move) (second move) scene :test #'equal))) moves)
      (setq Ti (get-prev-time Ti)))
    scene)
) ; END reconstruct-scene


(defun compute-relations (scene)
; ```````````````````````````````
; Computes all spatial relations that hold at a particular scene
; NOTE: we assume uniqueness of coords in the scene, or else this will break
;
  (remove nil (mapcan (lambda (coords1) (mapcan (lambda (coords2)
    (if (not (equal (car coords1) (car coords2)))
      (mapcar (lambda (rel)
          (if (eval-relation rel coords1 coords2)
            (list (car coords1) rel (car coords2))))
        '(touching.p to-the-left-of.p to-the-right-of.p below.p above.p behind.p in-front-of.p on.p))))
    scene)) scene))
) ; END compute-relations


(defun get-time-of-relation (coords rel &key neg)
; `````````````````````````````````````````````````
; Get the most recent time at which a given relation held.
; If neg is given as t, get the most recent time at which the relation was not present.
;
  (labels ((get-time-of-relation-recur (rel Ti)
      (let* ((scene (reconstruct-scene2 coords Ti))
             (coords1 (find-car (first rel) scene)) (coords2 (find-car (third rel) scene))
             (rel-true (eval-relation (second rel) coords1 coords2)))
        (cond
          ((and neg (not rel-true)) Ti)
          ((and (not neg) rel-true) Ti)
          ((equal Ti 'T0) nil)
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
          ((member name moved-blocks) Ti)
          ((equal Ti 'T0) nil)
          (t (get-time-of-move-recur name (get-prev-time Ti)))))))
    (get-time-of-move-recur name (get-prev-time *time*)))
) ; END get-time-of-move


(defun get-moves-at-time (Ti)
; ````````````````````````````
; Gets the moves which happened during Ti
;
  (mapcar (lambda (move) `((,(caar move) (past move.v)) 1)) (extract-moves (gethash Ti *context*)))
) ; END get-moves-at-time


(defun get-moves-at-times (T-list)
; ```````````````````````````````
; Gets the moves which happened during the times in T-list.
;
  (mapcan #'get-moves-at-time T-list)
) ; END get-moves-at-times


(defun eval-relation-time (coords rel Ti &key neg)
; ``````````````````````````````````````````````````
; Determines whether a relation holds at a particular time.
; If neg is given as t, return t if the relation doesn't hold at that time.
;
  (let* ((scene (reconstruct-scene2 coords Ti))
         (coords1 (find-car (first rel) scene)) (coords2 (find-car (third rel) scene))
         (rel-true (eval-relation (second rel) coords1 coords2)))
    (if (or (and neg (not rel-true)) (and (not neg) rel-true)) t))
) ; END eval-relation-time


(defun eval-relation-all-time (coords rel &key neg)
; ```````````````````````````````````````````````````
; Determines whether a relation holds over all times.
; If neg is given as t, return t if *the relation doesn't hold at all times*.
;
  (labels ((eval-relation-all-time-recur (rel Ti)
      (let ((rel-true (eval-relation-time coords rel Ti)))
        (cond
          ((equal Ti 'T0) rel-true)
          (t (and rel-true (eval-relation-all-time-recur rel (get-prev-time Ti))))))))
    (let ((result (eval-relation-all-time-recur rel (get-prev-time *time*))))
      (if (or (and neg (not result)) (and (not neg) result)) t)))
) ; END eval-relation-all-time


(defun eval-relation-no-time (coords rel &key neg)
; ```````````````````````````````````````````````````
; Determines whether a relation never holds at any time.
; If neg is given as t, return t if *the relation holds at some times*.
;
  (labels ((eval-relation-all-time-recur (rel Ti)
      (let ((rel-true (eval-relation-time coords rel Ti :neg t)))
        (cond
          ((equal Ti 'T0) rel-true)
          (t (and rel-true (eval-relation-all-time-recur rel (get-prev-time Ti))))))))
    (let ((result (eval-relation-all-time-recur rel (get-prev-time *time*))))
      (if (or (and neg (not result)) (and (not neg) result)) t)))
) ; END eval-relation-no-time


(defun count-moves (&key Ti block)
; ``````````````````````````````````
; Lists all moves.
; If Ti is given, lists all moves since Ti.
; If block is given, lists all moves with block as the subject.
; 
  (if (null Ti) (setq Ti 'T0))
  
) ; END count-moves


(defun get-turns-ago (n)
; ````````````````````````
; Gets the constant denoting the time n turns ago.
;
  (intern (format nil "T~a" (max 0 (- (chars-to-int (cdr (explode *time*))) n))))
) ; END get-turns-ago


(defun compare-time (Ti Tj)
; ``````````````````````````
; Returns -1 if Ti is before Tj, 0 if they're the same, 1 if Ti is after Tj
;
  (let ((i (chars-to-int (cdr (explode Ti)))) (j (chars-to-int (cdr (explode Tj)))))
    (if (= i j) 0
      (if (> i j) 1 -1)))
) ; END compare-time


(defun get-next-time (Ti)
; `````````````````````````
; Get a constant denoting the subsequent period (related by before.p/after.p propositions in context).
;
  (third (car (remove-if-not #'before-prop? (gethash Ti *context*))))
) ; END get-next-time


(defun get-prev-time (Ti)
; `````````````````````````
; Get a constant denoting the previous period (related by before.p/after.p propositions in context).
;
  (third (car (remove-if-not #'after-prop? (gethash Ti *context*))))
) ; END get-prev-time


(defun get-times-after (Ti n)
; ````````````````````````````
; Gets a list of all times after Ti going back n hops (if n=-1, get all times).
;
  (let ((next-time (get-next-time Ti)))
    (cond
      ((or (null next-time) (= n 0)) nil)
      (t (cons next-time (get-times-after next-time (- n 1))))))
) ; END get-times-after


(defun get-times-before (Ti n)
; ``````````````````````````````
; Gets a list of all times before Ti going back n hops (if n=-1, get all times).
;
  (let ((prev-time (get-prev-time Ti)))
    (cond
      ((or (null prev-time) (= n 0)) nil)
      (t (cons prev-time (get-times-before prev-time (- n 1))))))
) ; END get-times-before


(defun get-times-final (n)
; ``````````````````````````
; Gets a list of all times, going backward n hops.
;
  (get-times-before *time* n)
) ; END get-times-final


(defun get-times-init (n)
; ````````````````````````
; Gets the initial time(s), going forward n hops.
; NOTE: should this assume initial time always 'T0, or
; use recursive method from *time* to find initial time?
;
  (cons 'T0 (get-times-after 'T0 (- n 1)))
) ; END get-times-init


(defun get-time-nth (n)
; ``````````````````````
; Gets the nth time since the beginning.
;
  (last (get-times-init (+ 1 n)))
) ; END get-time-nth


(defun get-time-current ()
; `````````````````````````
; Gets the current time.
; NOTE: for now, this is just the value of the parameter *time*.
;
  *time*
) ; END get-time-current


; TTT flags and other preds are defined as follows
; ``````````````````````````````````````````````````
(defun hist-adv-prev? (ulf)
  (member ulf '(previously.adv-e before.adv-e)))
(defun hist-adv-next? (ulf)
  (member ulf '(since.adv-e)))
(defun hist-adv-always? (ulf)
  (member ulf '(always.adv-e)))
(defun hist-adv-ever? (ulf)
  (member ulf '(ever.adv-e)))
(defun hist-adv-init? (ulf)
  (member ulf '(originally.adv-e initially.adv-e)))
(defun hist-adv-cur? (ulf)
  (member ulf '(currently.adv-e)))
(defun hist-adv-recent? (ulf)
  (member ulf '(recently.adv-e)))

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
  (member ulf '(before.p prior_to.p preceding.p)))
(defun hist-prep-next? (ulf)
  (member ulf '(after.p following.p since.p)))

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