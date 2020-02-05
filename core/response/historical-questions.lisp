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


(defun resolve-adv-e-phrase (ulf)
; `````````````````````````````````
; Resolves a phrasal adv-e (e.g. "before I moved it") into a list of times.
;
  (let ((adv-e (cadr (extract-adv-e-phrase ulf))))
    (cond
      ((prep-conjunction? adv-e)
        (intersection (resolve-adv-e-phrase (first adv-e)) (resolve-adv-e-phrase (third adv-e))))
      ((reified-event? (second adv-e))
        
        )
      (())
    )
  )
) ; END resolve-adv-e-phrase


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


;; (defun reconstruct-scene2 (coords Tn)
;; ; `````````````````````````````````````
;; ; Reconstruct the scene (i.e. a list of at-loc.p propositions for each block)
;; ; at the time denoted by Tn.
;; ;
;;   (let* ((Ti (get-prev-time *time*)) (scene (extract-coords (gethash Ti *context*))) moves)
;;     (loop while (and Ti (> (compare-time Ti Tn) -1) (> (compare-time Ti 'T0) -1)) do
;;       (setq moves (extract-moves (gethash Ti *context*)))
;;       (mapcar (lambda (move)
;;         (setq scene (subst (first move) (second move) scene :test #'equal))) moves)
;;       (setq Ti (get-prev-time Ti)))
;;     scene)
;; ) ; END reconstruct-scene2


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


(defun time-of-relation (coords rel &key neg)
; ````````````````````````````````````````````
; Get the most recent time at which a given relation held.
; If neg is given as t, get the most recent time at which the relation was not present.
;
  (labels ((time-of-relation-recur (rel T-cur)
      (let* ((scene (reconstruct-scene2 coords T-cur))
             (coords1 (find-car (first rel) scene)) (coords2 (find-car (third rel) scene))
             (rel-true (eval-relation (second rel) coords1 coords2)))
        (cond
          ((and neg (not rel-true)) T-cur)
          ((and (not neg) rel-true) T-cur)
          ((equal T-cur 'T0) nil)
          (t (time-of-relation-recur rel (get-prev-time T-cur)))))))
    (time-of-relation-recur rel (get-prev-time *time*)))
) ; END time-of-relation


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
  (labels ((eval-relation-all-time-recur (rel T-cur)
      (let ((rel-true (eval-relation-time coords rel T-cur)))
        (cond
          ((equal T-cur 'T0) rel-true)
          (t (and rel-true (eval-relation-all-time-recur rel (get-prev-time T-cur))))))))
    (let ((result (eval-relation-all-time-recur rel (get-prev-time *time*))))
      (if (or (and neg (not result)) (and (not neg) result)) t)))
) ; END eval-relation-all-time


(defun eval-relation-no-time (coords rel &key neg)
; ```````````````````````````````````````````````````
; Determines whether a relation never holds at any time.
; If neg is given as t, return t if *the relation holds at some times*.
;
  (labels ((eval-relation-all-time-recur (rel T-cur)
      (let ((rel-true (eval-relation-time coords rel T-cur :neg t)))
        (cond
          ((equal T-cur 'T0) rel-true)
          (t (and rel-true (eval-relation-all-time-recur rel (get-prev-time T-cur))))))))
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


