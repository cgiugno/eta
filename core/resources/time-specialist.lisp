(defun update-time ()
; ``````````````````````
; Updates time to a "new period", i.e. creates a new constant denoting
; a new time period (and stores before/after relationships in context)
;
  (let ((time-old *time*) time-new pred-before pred-after)
    (setq time-new (intern (format nil "NOW~a"
      (1+ (chars-to-int (cdddr (explode *time*)))))))
    (setq *time* time-new)
    (store-time)
    (setq pred-before (list time-old 'before.p time-new))
    (setq pred-after  (list time-new 'after.p time-old))
    (store-fact pred-before *context* :keys (list (car pred-before)))
    (store-fact pred-after  *context* :keys (list (car pred-after))))
) ; END update-time


(defun get-time ()
; ``````````````````
; Gets the system time and returns a record structure. The format of the
; record is ($ time ?second ?minute ?hour ?day ?month ?year).
; 
  (multiple-value-bind (seconds minutes hours days months years) (get-decoded-time)
    `($ date-time ,years ,months ,days ,hours ,minutes ,seconds))
) ; END get-time


(defun to-universal-time (time-record)
; ``````````````````````````````````````
; Converts time record to universal time.
;
  (apply #'encode-universal-time (reverse (cddr time-record)))
) ; END to-universal-time


(defun store-time ()
; ````````````````````
; Gets and stores the date-time of the current time proposition.
;
  (let ((pred-time (list *time* 'at-about.p (get-time))))
    (store-fact pred-time *context* :keys (list (car pred-time))))
) ; END store-time


(defun get-elapsed-time (time-record)
; ````````````````````````````````````
; Gets the elapsed time between time-record and the current time as a ULF noun phrase (up to hours).
;
  (let ((seconds (- (get-universal-time) (to-universal-time time-record))) minutes hours)
    (if (> seconds 60)
      (setq minutes (floor (/ seconds 60))))
    (if (> seconds (* 60 60))
      (setq hours (floor (/ seconds (* 60 60)))))
    (cond
      (hours (list (num-to-det hours) (if (> hours 1) '(plur hour.n) 'hour.n)))
      (minutes (list (num-to-det minutes) (if (> minutes 1) '(plur minute.n) 'minute.n)))
      (seconds (list (num-to-det seconds) (if (> seconds 1) '(plur second.n) 'second.n)))))
) ; END get-elapsed-time


(defun get-time-of-episode (ep-sym)
; ``````````````````````````````````
; Gets the time record corresponding to an episode symbol.
;
  (third (car (remove-if-not #'at-about-prop? (gethash ep-sym *context*))))
) ; END get-time-of-episode


(defun compare-time (Ti Tj)
; ``````````````````````````
; Returns -1 if Ti is before Tj, 0 if they're the same, 1 if Ti is after Tj
;
  (let ((i (chars-to-int (cdddr (explode Ti)))) (j (chars-to-int (cdddr (explode Tj)))))
    (if (= i j) 0
      (if (> i j) 1 -1)))
) ; END compare-time


(defun diff-times (Ti Tj)
; `````````````````````````
; Returns the difference between time Ti and Tj
;
  (let ((i (chars-to-int (cdddr (explode Ti)))) (j (chars-to-int (cdddr (explode Tj)))))
    (abs (- i j)))
) ; END diff-times


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


(defun most-recent (times)
; ``````````````````````````
; Gets the most recent time.
; NOTE: for now, this assumes that the symbols representing times are in alphanumerical
; order, e.g. NOW5 > NOW1. This is probably not ideal, since it relies on internal representations...
;
  (let ((most (car (sort (copy-seq times) (lambda (x y) (> (compare-time x y) 0))))))
    (if most (list most)))
) ; END most-recent


(defun least-recent (times)
; ``````````````````````````
; Gets the least recent time.
; NOTE: for now, this assumes that the symbols representing times are in alphanumerical
; order, e.g. NOW5 > NOW1. This is probably not ideal, since it relies on internal representations...
;
  (let ((least (car (sort (copy-seq times) (lambda (x y) (< (compare-time x y) 0))))))
    (if least (list least)))
) ; END least-recent


(defun time-inclusive (times)
; ````````````````````````````
; Makes list of times inclusive by including previous time, e.g. (NOW1 NOW2 NOW3) => (NOW0 NOW1 NOW3 NOW4)
;
  (let* ((least (least-recent times)) (prev (get-prev-time (car least))))
    (append (if prev (list prev)) times))
) ; END time-inclusive