(defun update-time ()
;``````````````````````
; Updates time to a "new period", i.e. creates a new constant denoting
; a new time period (and stores before/after relationships in context)
;
  (let ((time-old *time*) time-new pred-before pred-after)
    (setq time-new (intern (format nil "NOW~a"
      (1+ (chars-to-int (cdddr (explode *time*)))))))
    (setq *time* time-new)
    (setq pred-before (list time-old 'before.p time-new))
    (setq pred-after  (list time-new 'after.p time-old))
    (store-fact pred-before *context* :keys (list (car pred-before)))
    (store-fact pred-after  *context* :keys (list (car pred-after))))
) ; END update-time


(defun compare-time (Ti Tj)
; ``````````````````````````
; Returns -1 if Ti is before Tj, 0 if they're the same, 1 if Ti is after Tj
;
  (let ((i (chars-to-int (cdddr (explode Ti)))) (j (chars-to-int (cdddr (explode Tj)))))
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
; NOTE: should this assume initial time always 'NOW0, or
; use recursive method from *time* to find initial time?
;
  (cons 'NOW0 (get-times-after 'NOW0 (- n 1)))
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