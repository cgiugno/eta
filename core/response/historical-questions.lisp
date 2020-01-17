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




(defun recall-answer (coords ulf)
; `````````````````````````````````
; Given current observed block coordinates and the ULF of the query, recall the answer by consulting
; historical record of block moves stored in context
;
  '(|Texaco| to-the-left-of.p |Twitter|)
) ; END recall-answer



(defun loc-record? (list)
; `````````````````````````
; Checks whether a list is a location record of form ($ loc ?x ?y ?z)
;
  (and (listp list) (= (length list) 5) (equal '$ (first list)) (equal 'loc (second list))
    (every #'numberp (cddr list)))
) ; END loc-record?



(defun at-loc-prop? (prop)
; ```````````````````````````
; Checks whether a proposition is an at-loc.p formula.
; i.e. (|Twitter| at-loc.p ($ loc ?x ?y ?z))
;
  (and (listp prop) (= (length prop) 3) (equal (second prop) 'at-loc.p) (loc-record? (third prop)))
) ; END at-loc-prop?



(defun move-prop? (prop)
; `````````````````````````
; Checks whether a proposition is a move.v formula.
; i.e. (|Toyota| ((past move.v) (from.p-arg ($ loc ?x1 ?y1 ?z1)) (to.p-arg ($ loc ?x2 ?y2 ?z2))))
;
  (and (listp prop) (= (length prop) 2) (listp (second prop)) (= (length (second prop)) 3)
    (equal (first (second prop)) '(past move.v)) (listp (second (second prop))) (listp (third (second prop)))
    (loc-record? (second (second (second prop)))) (loc-record? (second (third (second prop)))))
) ; END move-prop?



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



;; (defun at-coords-prop? (prop)
;; ; ````````````````````````````
;; ; Checks whether a proposition is an at-coords.p formula.
;; ;
;;   (and (listp prop) (= (length prop) 5) (equal (second prop) 'at-coords.p))
;; ) ; END at-coords-prop?



(defun after-prop? (prop)
; ````````````````````````````
; Checks whether a proposition is an after relation, i.e. after.p.
;
  (and (listp prop) (>= (length prop) 2) (equal (second prop) 'after.p))
) ; END after-prop?



(defun before-prop? (prop)
; ````````````````````````````
; Checks whether a proposition is an before relation, i.e. before.p.
;
  (and (listp prop) (>= (length prop) 2) (equal (second prop) 'before.p))
) ; END before-prop?



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



(defun reconstruct-scene2 (coords Tn)
; `````````````````````````````````````
; Reconstruct the scene (i.e. a list of at-loc.p propositions for each block)
; at the time denoted by Tn, given current coordinates.
;
  (let* ((Ti (get-prev-time *time*)) (scene coords) moves)
    (loop while (and Ti (> (compare-time Ti Tn) -1) (> (compare-time Ti 'T0) -1)) do
      (setq moves (extract-moves (gethash Ti *context*)))
      (mapcar (lambda (move)
        (setq scene (subst (first move) (second move) scene :test #'equal))) moves)
      (setq Ti (get-prev-time Ti)))
    scene)
) ; END reconstruct-scene



(defun reconstruct-scene (Tn)
; ````````````````````````````
; Reconstruct the scene (i.e. a list of at-loc.p propositions for each block)
; at the time denoted by Tn.
;
  (let* ((Ti (get-prev-time *time*)) (scene (extract-coords (gethash Ti *context*))) moves)
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



(defun time-of-relation (rel &key neg)
; ``````````````````````````````````````
; Get the most recent time at which a given relation held.
; If neg is given as t, get the most recent time at which the relation was not present.
;
  (labels ((time-of-relation-recur (rel T-cur)
      (let* ((scene (reconstruct-scene T-cur))
             (coords1 (find-car (first rel) scene)) (coords2 (find-car (third rel) scene))
             (rel-true (eval-relation (second rel) coords1 coords2)))
        (cond
          ((and neg (not rel-true)) T-cur)
          ((and (not neg) rel-true) T-cur)
          ((equal T-cur 'T0) nil)
          (t (time-of-relation-recur rel (get-prev-time T-cur)))))))
    (time-of-relation-recur rel (get-prev-time *time*)))
) ; END time-of-relation



(defun eval-relation-time (rel Ti &key neg)
; ``````````````````````````````````````````
; Determines whether a relation holds at a particular time.
; If neg is given as t, return t if the relation doesn't hold at that time.
;
  (let* ((scene (reconstruct-scene Ti))
         (coords1 (find-car (first rel) scene)) (coords2 (find-car (third rel) scene))
         (rel-true (eval-relation (second rel) coords1 coords2)))
    (if (or (and neg (not rel-true)) (and (not neg) rel-true)) t))
) ; END eval-relation-time



(defun eval-relation-all-time (rel &key neg)
; ````````````````````````````````````````````
; Determines whether a relation holds over all times.
; If neg is given as t, return t if the relation does not hold at any time.
;
  (labels ((eval-relation-all-time-recur (rel T-cur)
      (let ((rel-true (eval-relation-time rel T-cur)))
        (cond
          ((equal T-cur 'T0) (if (or (and neg (not rel-true)) (and (not neg) rel-true)) t))
          (t (and (if (or (and neg (not rel-true)) (and (not neg) rel-true)) t)
                 (eval-relation-all-time-recur rel (get-prev-time T-cur))))))))
    (eval-relation-all-time-recur rel (get-prev-time *time*)))
) ; END eval-relation-all-time



;; (defun eval-relation-no-time (rel)
;; ; ```````````````````````````````````
;; ; Determines whether a relation holds at no time.
;; ;
;;   (labels ((eval-relation-all-time-recur (rel T-cur)
;;       (let ((relations (compute-relations (reconstruct-scene T-cur))))
;;         (cond
;;           ((equal T-cur 'T0) (if (not (member rel relations :test #'equal)) t))
;;           (t (and (not (member rel relations :test #'equal))
;;                  (eval-relation-all-time-recur rel (get-prev-time T-cur))))))))
;;     (eval-relation-all-time-recur rel (get-prev-time *time*)))
;; ) ; END eval-relation-no-time



