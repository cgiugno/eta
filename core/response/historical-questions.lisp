;; Dec 9/19
;; ================================================
;;
;; Functions used in answering historical questions
;;


; ((|Target| at-coords.p 0 0 0) (|Starbucks| at-coords.p 1 0 0) (|Twitter| at-coords.p 2 0 0) (|Texaco| at-coords.p 3 0 0) (|McDonalds| at-coords.p 4 0 0) (|Mercedes| at-coords.p 5 0 0) (|Toyota| at-coords.p 6 0 0) (|Burger King| at-coords.p 7 0 0))


(defun at-coords-prop? (prop)
; ````````````````````````````
; Checks whether a proposition is an at-coords.p formula.
;
  (and (listp prop) (= (length prop) 5) (equal (second prop) 'at-coords.p))
) ; END at-coords-prop?



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



(defun reconstruct-scene (Tn)
; ````````````````````````````
; Reconstruct the scene (i.e. a list of at-coord.p propositions for each block)
; at the time denoted by Tn.
;
  (let ((scene (remove-if-not #'at-coords-prop? (gethash 'T0 *context*)))
        (T-next (get-next-time 'T0)))
    (loop while (and T-next (< (compare-time T-next Tn) 1)) do
      (let ((update (remove-if-not #'at-coords-prop? (gethash T-next *context*))))
        (mapcar (lambda (prop) (setq scene (update-prop prop scene))) update))
      (setq T-next (get-next-time T-next)))
    scene)
) ; END reconstruct-scene



(defun time-of-relation (rel)
; ````````````````````````````
; Get the most recent time at which a given relation held.
; NOTE: probably won't work for negated relations, e.g. "when was the SRI block not on the Texaco block?"
;
  (labels ((time-of-relation-recur (rel T-cur)
      (let ((relations (compute-relations (reconstruct-scene T-cur))))
        (cond
          ((member rel relations :test #'equal) T-cur)
          ((equal T-cur 'T0) nil)
          (t (time-of-relation-recur rel (get-prev-time T-cur)))))))
    (time-of-relation-recur rel (get-prev-time *time*)))
) ; END time-of-relation



(defun eval-relation-time (rel Ti)
; `````````````````````````````````
; Determines whether a relation holds at a particular time.
;
  (let ((relations (compute-relations (reconstruct-scene Ti))))
    (if (member rel relations :test #'equal) t nil))
) ; END eval-relation-time



(defun eval-relation-all-time (rel)
; ```````````````````````````````````
; Determines whether a relation holds over all times.
;
  (labels ((eval-relation-all-time-recur (rel T-cur)
      (let ((relations (compute-relations (reconstruct-scene T-cur))))
        (cond
          ((equal T-cur 'T0) (if (member rel relations :test #'equal) t))
          (t (and (member rel relations :test #'equal)
                 (eval-relation-all-time-recur rel (get-prev-time T-cur))))))))
    (eval-relation-all-time-recur rel (get-prev-time *time*)))
) ; END eval-relation-all-time



(defun eval-relation-no-time (rel)
; ```````````````````````````````````
; Determines whether a relation holds at no time.
;
  (labels ((eval-relation-all-time-recur (rel T-cur)
      (let ((relations (compute-relations (reconstruct-scene T-cur))))
        (cond
          ((equal T-cur 'T0) (if (not (member rel relations :test #'equal)) t))
          (t (and (not (member rel relations :test #'equal))
                 (eval-relation-all-time-recur rel (get-prev-time T-cur))))))))
    (eval-relation-all-time-recur rel (get-prev-time *time*)))
) ; END eval-relation-no-time



(defun compute-relations (scene)
; ```````````````````````````````
; Computes all spatial relations that hold at a particular scene
; NOTE: we assume uniqueness of at-coords.p predicates in the scene, or else this will break
;
  (remove nil (mapcan (lambda (prop1) (mapcan (lambda (prop2)
    (if (not (equal (car prop1) (car prop2)))
      (mapcar (lambda (rel)
          (if (funcall rel (nth 2 prop1) (nth 3 prop1) (nth 4 prop1) (nth 2 prop2) (nth 3 prop2) (nth 4 prop2))
            (list (car prop1) rel (car prop2))))
        '(touching.p to-the-left-of.p to-the-right-of.p below.p above.p behind.p in-front-of.p on.p))
    )
  ) scene)) scene))
) ; END compute-relations